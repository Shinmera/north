(in-package #:org.shirakumo.north)

(defgeneric call (request &rest args))
(defgeneric call-signed (request consumer-secret &optional token-secret &rest args))
(defgeneric make-signed-request (client url method &key params headers oauth))
(defgeneric make-signed-data-request (client url data &key params headers oauth))
(defgeneric initiate-authentication (client))
(defgeneric complete-authentication (client verifier &optional token))

(defmethod call-signed ((request request) consumer-secret &optional token-secret &rest args)
  (apply #'call (make-authorized (make-signed request consumer-secret token-secret)) args))

(defclass client ()
  ((key :initarg :key :accessor key)
   (secret :initarg :secret :accessor secret)
   (token :initarg :token :accessor token)
   (token-secret :initarg :token-secret :accessor token-secret)
   (callback :initarg :callback :accessor callback)
   (request-token-uri :initarg :request-token-uri :accessor request-token-uri)
   (authorize-uri :initarg :authorize-uri :accessor authorize-uri)
   (access-token-uri :initarg :access-token-uri :accessor access-token-uri)
   (verify-uri :initarg :verify-uri :accessor verify-uri)
   (user-agent :initarg :user-agent :accessor user-agent))
  (:default-initargs
   :key (error "KEY required. This is your oAuth application's key.")
   :secret (error "SECRET required. This is your oAuth application's secret.")
   :token NIL
   :token-secret NIL
   :callback "oob"
   :request-token-uri (error "REQUEST-TOKEN-URI required.")
   :authorize-uri (error "AUTHORIZE-URI required.")
   :access-token-uri (error "ACCESS-TOKEN-URI required.")
   :verify-uri NIL
   :user-agent (user-agent T)))

(defmethod make-load-form ((client client) &optional env)
  (declare (ignore env))
  `(make-instance
    'client
    :key ,(key client)
    :secret ,(secret client)
    :token ,(token client)
    :token-secret ,(token-secret client)
    :callback ,(callback client)
    :request-token-uri ,(request-token-uri client)
    :authorize-uri ,(authorize-uri client)
    :access-token-uri ,(access-token-uri client)
    :verify-uri ,(verify-uri client)))

(defmethod make-signed-request ((client client) url method &key params headers oauth)
  (let ((request (make-request url method :params params :headers headers
                                          :oauth `(,@oauth
                                                   (:oauth_consumer_key . ,(key client))
                                                   (:oauth_token . ,(token client))))))
    (values (call-signed request (secret client) (token-secret client))
            request)))

(defmethod make-signed-data-request ((client client) url data &key params headers oauth)
  (let ((request (make-request url :post :headers headers
                                         :oauth `(,@oauth
                                                  (:oauth_consumer_key . ,(key client))
                                                  (:oauth_token . ,(token client))))))
    (make-authorized (make-signed request (secret client) (token-secret client)))
    (setf (parameters request)
          (append params
                  (loop for (k . v) in data
                        collect (destructuring-bind (file &key (content-type "application/octet-stream") filename)
                                    (if (consp v) v (list v))
                                  (list k file :content-type content-type :filename filename)))))
    (values (call request :form-data T :user-agent (user-agent client))
            request)))

(defmethod initiate-authentication ((client client))
  (setf (token client) NIL)
  (setf (token-secret client) NIL)
  (multiple-value-bind (result request) (make-signed-request client (request-token-uri client) :post
                                                             :oauth `((:oauth_callback . ,(callback client))))
    (let ((result (oauth-response->alist result)))
      (unless (string-equal "true" (pget :oauth_callback_confirmed result))
        (error 'callback-unconfirmed :request request))
      (setf (token client) (pget :oauth_token result))
      (setf (token-secret client) (pget :oauth_token_secret result))
      (format NIL "~a?oauth_token=~a"
              (authorize-uri client) (url-encode (token client))))))

(defmethod complete-authentication ((client client) verifier &optional (token (token client)))
  (let ((result (make-signed-request client (access-token-uri client) :post
                                     :oauth `((:oauth_verifier . ,verifier)
                                              (:oauth_token . ,token)))))
    (let ((result (oauth-response->alist result)))
      (setf (token client) (pget :oauth_token result))
      (setf (token-secret client) (pget :oauth_token_secret result))
      (when (verify-uri client)
        (make-signed-request client (verify-uri client) :post))
      client)))
