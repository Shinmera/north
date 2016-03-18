#|
 This file is a part of north
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.north)

(defgeneric call-request (request))
(defgeneric call-signed (request consumer-secret &optional token-secret))
(defgeneric make-signed-request (client url method &key get post headers oauth))
(defgeneric initiate-authentication (client))
(defgeneric complete-authentication (client verifier &optional token))

(defmethod call-request ((request request))
  (let ((drakma:*text-content-types*
          (list* '("application" . "x-www-form-urlencoded")
                 drakma:*text-content-types*)))
    (multiple-value-bind (body status-code headers)
        (drakma:http-request
         (url request)
         :method (http-method request)
         :parameters (append (get-params request)
                             (post-params request))
         :additional-headers (headers request)
         :url-encoder #'url-encode
         :external-format-in *external-format*
         :external-format-out *external-format*)
      (if (= status-code 200)
          (oauth-response->alist body)
          (error 'request-failed :request request :body body :status-code status-code :headers headers)))))

(defmethod call-signed ((request request) consumer-secret &optional token-secret)
  (call-request (make-authorized (make-signed request consumer-secret token-secret))))

(defclass client ()
  ((key :initarg :key :accessor key)
   (secret :initarg :secret :accessor secret)
   (token :initarg :token :accessor token)
   (token-secret :initarg :token-secret :accessor token-secret)
   (callback :initarg :callback :accessor callback)
   (request-token-uri :initarg :request-token-uri :accessor request-token-uri)
   (authorize-uri :initarg :authorize-uri :accessor authorize-uri)
   (access-token-uri :initarg :access-token-uri :accessor access-token-uri)
   (verify-uri :initarg :verify-uri :accessor verify-uri))
  (:default-initargs
   :key (error "KEY required. This is your oAuth application's key.")
   :secret (error "SECRET required. This is your oAuth application's secret.")
   :token NIL
   :token-secret NIL
   :callback "oob"
   :request-token-uri (error "REQUEST-TOKEN-URI required.")
   :authorize-uri (error "AUTHORIZE-URI required.")
   :access-token-uri (error "ACCESS-TOKEN-URI required.")
   :verify-uri NIL))

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

(defmethod make-signed-request ((client client) url method &key get post headers oauth)
  (let ((request (make-request url method :get get :post post :headers headers
                                          :oauth `(,@oauth
                                                   (:oauth_consumer_key . ,(key client))
                                                   (:oauth_token . ,(token client))))))
    (values (call-signed request (secret client) (token-secret client))
            request)))

(defmethod initiate-authentication ((client client))
  (setf (token client) NIL)
  (setf (token-secret client) NIL)
  (multiple-value-bind (result request) (make-signed-request client (request-token-uri client) :post
                                                             :oauth `((:oauth_callback . ,(callback client))))
    (unless (string-equal "true" (pget :oauth_callback_confirmed result))
      (error 'callback-unconfirmed :request request))
    (setf (token client) (pget :oauth_token result))
    (setf (token-secret client) (pget :oauth_token_secret result))
    (format NIL "~a?oauth_token=~a"
            (authorize-uri client) (url-encode (token client)))))

(defun complete-authentication ((client client) verifier &optional (token (token client)))
  (let ((result (make-signed-request client (access-token-uri client) :post
                                     :oauth `((:oauth_verifier . ,verifier)
                                              (:oauth_token . ,token)))))
    (setf (token client) (pget :oauth_token result))
    (setf (token-secret client) (pget :oauth_token_secret result))
    (when (verify-uri client)
      (make-signed-request client (verify-uri client) :post))
    client))
