#|
 This file is a part of north
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.north)

(defgeneric make-consumer (server &key &allow-other-keys))
(defgeneric make-session (server consumer callback &key access &allow-other-keys))
(defgeneric consumer (server consumer-key))
(defgeneric session (server token))
(defgeneric rehash-session (server session))
(defgeneric revoke-consumer (server consumer-key))
(defgeneric revoke-session (server token))
(defgeneric record-nonce (server timestamp nonce))
(defgeneric find-nonce (server timestamp nonce))
(defgeneric oauth/request-token (server request))
(defgeneric oauth/authorize (server request))
(defgeneric oauth/access-token (server request))
(defgeneric oauth/verify (server request))

(defclass session ()
  ((token :initarg :token :accessor token)
   (token-secret :initarg :token-secret :accessor token-secret)
   (verifier :initarg :verifier :accessor verifier)
   (callback :initarg :callback :accessor callback)
   (key :initarg :key :accessor key)
   (access :initarg :access :accessor access))
  (:default-initargs
   :token (make-nonce)
   :secret (make-nonce)
   :verifier (make-nonce)
   :callback "oob"
   :key (error "KEY required.")
   :access :request))

(defclass consumer ()
  ((key :initarg :key :accessor key)
   (secret :initarg :secret :accessor secret)
   (name :initarg :name :accessor name))
  (:default-initargs
   :key (make-nonce)
   :secret (make-nonce)
   :name (error "NAME required.")))

(defclass server ()
  ())

(defmethod make-session ((server server) (consumer-key string) callback &rest args)
  (apply #'make-session server (consumer server consumer-key) callback args))

(defmethod rehash-session ((server server) (string string))
  (rehash-session server (session server string)))

(defmethod revoke-consumer ((server server) (string string))
  (revoke-consumer server (consumer server string)))

(defmethod revoke-session ((server server) (string string))
  (revoke-session server (session server string)))

(defmethod find-nonce ((server server) (timestamp string) nonce)
  (find-nonce server (parse-integer timestamp) nonce))

(defun check-parameters-present (request &rest parameters)
  (loop for param in parameters
        unless (pget param (oauth request))
        collect param into missing
        finally (when missing (error 'parameters-missing :request request :parameters missing))))

(defun check-nonce (request server)
  (let ((nonce (pget :oauth_nonce (oauth request)))
        (timestamp (pget :oauth_timestamp (oauth request))))
    (if (find-nonce server timestamp nonce)
        (error 'nonce-reused :request request)
        (record-nonce server timestamp nonce))))

(defun check-version (request)
  (let ((version (pget :oauth_version (oauth request))))
    (unless (or (not version) (string= "1.0" version))
      (error 'bad-version :request request))))

(defun check-request (request server &optional secret)
  (let* ((consumer-key (pget :oauth_consumer_key (oauth request)))
         (consumer (consumer server consumer-key)))
    (check-version request)
    (check-nonce request server)
    (unless consumer
      (error 'invalid-consumer :request request))
    (unless (verify request (secret (consumer server consumer-key)) secret)
      (error 'invalid-signature :request request))))

(defun check-verifier (request server)
  (let ((verifier (pget :oauth_verifier (oauth request)))
        (session (session server (pget :oauth_token (oauth request)))))
    (unless (and session (string= verifier (verifier session)))
      (error 'invalid-verifier :request request))))

(defun check-token (request server)
  (let ((session (session server (or (pget :oauth_token (oauth request))
                                     (pget "oauth_token" (get-params request))))))
    (unless (and session (token session))
      (error 'invalid-token :request request))))

(defmethod oauth/request-token ((server server) (request request))
  (check-parameters-present request
    :oauth_consumer_key :oauth_signature_method :oauth_signature
    :oauth_timestamp :oauth_nonce :oauth_callback)
  (check-request request server)
  (let ((session (make-session server
                               (pget :oauth_consumer_key (oauth request))
                               (pget :oauth_callback (oauth request))
                               :access :request)))
    (values (token session) (token-secret session) T)))

(defmethod oauth/authorize ((server server) (request request))
  (check-token request server)
  (let* ((session (session server (pget "oauth_token" (get-params request))))
         (verifier (verifier session))
         (token (token session))
         (callback (callback session)))
    (unless verifier
      (error 'verifier-taken :request request))
    ;; Invalidate verifier
    (setf (verifier session) NIL)
    (values token verifier
            (when (string/= callback "oob")
              (format NIL "~a?oauth_token=~a&oauth_verifier=~a"
                      callback (url-encode token) (url-encode verifier))))))

(defmethod oauth/access-token ((server server) (request request))
  (check-parameters-present request
    :oauth_consumer_key :oauth_signature_method :oauth_signature
    :oauth_timestamp :oauth_nonce :oauth_token :oauth_verifier)
  (check-token request server)
  (check-verifier request server)
  (let ((session (session server (pget :oauth_token (oauth request)))))
    (check-request request server)
    (setf (access session) :access)
    (rehash-session server session)
    (values (token session) (token-secret session))))

(defmethod oauth/verify ((server server) (request request))
  (check-parameters-present request
    :oauth_consumer_key :oauth_signature_method :oauth_signature
    :oauth_timestamp :oauth_nonce :oauth_token)
  (check-token request server)
  (check-request request server)
  T)

(defclass simple-server (server)
  ((consumers :initform (make-hash-table :test 'equal) :accessor consumers)
   (sessions :initform (make-hash-table :test 'equal) :accessor sessions)
   (nonces :initform (make-hash-table :test 'eql) :accessor nonces)))

(defmethod make-consumer ((server simple-server) &key name)
  (let ((consumer (make-instance 'consumer :name name)))
    (setf (gethash (key consumer) (consumers server)) consumer)
    consumer))

(defmethod make-session ((server simple-server) (consumer consumer) callback &key (access :request))
  (let ((session (make-instance 'session :key (key consumer) :access access)))
    (setf (gethash (token session) (sessions server)) session)
    session))

(defmethod consumer ((server simple-server) consumer-key)
  (gethash consumer-key (consumers server)))

(defmethod session ((server simple-server) token)
  (gethash token (sessions server)))

(defmethod rehash-session ((server simple-server) (session session))
  (revoke-session server session)
  (setf (token session) (make-nonce))
  (setf (token-secret session) (make-nonce))
  (setf (gethash (token session) (sessions server)) session))

(defmethod revoke-consumer ((server simple-server) (consumer consumer))
  (remhash (key consumer) (consumers server))
  consumer)

(defmethod revoke-session ((server simple-server) (session session))
  (remhash (token session) (sessions server))
  session)

(defmethod record-nonce ((server simple-server) timestamp nonce)
  (push nonce (gethash timestamp (nonces server)))
  nonce)

(defmethod find-nonce ((server simple-server) (timestamp integer) (nonce string))
  (find nonce (gethash timestamp (nonces server)) :test #'equal))

