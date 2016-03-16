#|
 This file is a part of north
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.north)

(defgeneric make-consumer (server &key &allow-other-keys))
(defgeneric make-request-token (server))
(defgeneric make-verifier-token (server request-token))
(defgeneric make-access-token (server))
(defgeneric store-callback (server request-token callback))
(defgeneric consumer (server consumer-key))
(defgeneric request-secret (server request-token))
(defgeneric verifier-token (server request-token))
(defgeneric callback (server request-token))
(defgeneric access-secret (server access-token))
(defgeneric revoke-consumer (server consumer-key))
(defgeneric revoke-request-token (server request-token))
(defgeneric revoke-verifier-token (server request-token))
(defgeneric revoke-access-token (server access-token))
(defgeneric record-nonce (server timestamp nonce))
(defgeneric find-nonce (server timestamp nonce))
(defgeneric oauth/request-token (server request))
(defgeneric oauth/authorize (server request))
(defgeneric oauth/access-token (server request))
(defgeneric oauth/verify (server request))

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

(defmethod revoke-request-token :after ((server server) request-token)
  (when (verifier-token server request-token)
    (revoke-verifier-token server request-token)))

(defun check-parameters-present (request &rest parameters)
  (loop for param in parameters
        unless (pget param (oauth request))
        collect param into missing
        finally (when missing (error 'parameters-missing :request request :parameters missing))))

(defun check-nonce (request server)
  (let ((nonce (pget :oauth_nonce (oauth request)))
        (timestamp (parse-integer (pget :oauth_timestamp (oauth request)))))
    (if (find-nonce server timestamp nonce)
        (error 'nonce-reused :request request)
        (record-nonce server timestamp nonce))))

(defun check-version (request)
  (let ((version (pget :oauth_version (oauth request))))
    (unless (or (not version) (string= "1.0" version))
      (error 'bad-version :request request))))

(defun check-request (request server)
  (let* ((consumer-key (pget :oauth_consumer_key (oauth request)))
         (token (pget :oauth_token (oauth request)))
         (consumer (consumer server consumer-key)))
    (check-version request)
    (check-nonce request server)
    (unless consumer
      (error 'invalid-consumer :request request))
    (unless (verify request
                    (secret (consumer server consumer-key))
                    (or (access-secret server token)
                        (request-secret server token)))
      (error 'invalid-signature :request request))))

(defun check-verifier (request server)
  (let ((verifier (pget :oauth_verifier (oauth request)))
        (request-token (pget :oauth_token (oauth request))))
    (unless (string= verifier (verifier-token server request-token))
      (error 'invalid-verifier :request request))))

(defun check-access-secret (request server)
  (unless (access-secret server (pget :oauth_token (oauth request)))
    (error 'invalid-token :request request)))

(defun check-request-token (request server)
  (unless (request-secret server (pget "oauth_token" (get-params request)))
    (error 'invalid-token :request request)))

(defmethod oauth/request-token ((server server) (request request))
  (check-parameters-present request
    :oauth_consumer_key :oauth_signature_method :oauth_signature
    :oauth_timestamp :oauth_nonce :oauth_callback)
  (check-request request server)
  (multiple-value-bind (request-token request-secret) (make-request-token server)
    (store-callback server request-token (pget :oauth_callback (oauth request)))
    (values request-token request-secret T)))

(defmethod oauth/authorize ((server server) (request request))
  (let ((request-token (pget "oauth_token" (get-params request))))
    (check-request-token request server)
    (when (verifier-token server request-token)
      (error 'verifier-taken :request request))
    (let ((verifier (make-verifier-token server request-token))
          (callback (callback server request-token)))
      (values request-token verifier
              (when (string/= callback "oob")
                (format NIL "~a?oauth_token=~a&oauth_verifier=~a"
                        callback (url-encode request-token) (url-encode verifier)))))))

(defmethod oauth/access-token ((server server) (request request))
  (check-parameters-present request
    :oauth_consumer_key :oauth_signature_method :oauth_signature
    :oauth_timestamp :oauth_nonce :oauth_token :oauth_verifier)
  (check-request request server)
  (check-verifier request server)
  (multiple-value-bind (access-token access-secret) (make-access-token server)
    (revoke-request-token server (pget :oauth_token (oauth request)))
    (values access-token access-secret)))

(defmethod oauth/verify ((server server) (request request))
  (check-parameters-present request
    :oauth_consumer_key :oauth_signature_method :oauth_signature
                        :oauth_timestamp :oauth_nonce :oauth_token)
  (check-access-secret request server)
  (check-request request server)
  T)

(defclass simple-server (server)
  ((consumers :initform (make-hash-table :test 'equal) :accessor consumers)
   (request-tokens :initform (make-hash-table :test 'equal) :accessor request-tokens)
   (verifier-tokens :initform (make-hash-table :test 'equal) :accessor verifier-tokens)
   (access-tokens :initform (make-hash-table :test 'equal) :accessor access-tokens)
   (nonces :initform (make-hash-table :test 'eql) :accessor nonces)))

(defmethod make-consumer ((server simple-server) &key name)
  (let ((consumer (make-instance 'consumer :name name)))
    (setf (gethash (key consumer) (request-tokens server)) consumer)
    consumer))

(defmethod make-request-token ((server simple-server))
  (let ((token (make-nonce))
        (secret (make-nonce)))
    (setf (gethash token (request-tokens server)) secret)
    (values token secret)))

(defmethod make-verifier-token ((server simple-server) request-token)
  (let ((verifier-token (make-nonce)))
    (setf (gethash request-token (request-tokens server)) verifier-token)
    verifier-token))

(defmethod make-access-token ((server simple-server))
  (let ((token (make-nonce))
        (secret (make-nonce)))
    (setf (gethash token (access-tokens server)) secret)
    (values token secret)))

(defmethod consumer ((server simple-server) consumer-key)
  (gethash consumer-key (consumers server)))

(defmethod request-secret ((server simple-server) request-token)
  (gethash request-token (request-tokens server)))

(defmethod verifier-token ((server simple-server) request-token)
  (gethash request-token (verifier-tokens server)))

(defmethod access-secret ((server simple-server) access-token)
  (gethash access-token (access-tokens server)))

(defmethod revoke-consumer ((server simple-server) consumer-key)
  (remhash consumer-key (consumers server)))

(defmethod revoke-request-token ((server simple-server) request-token)
  (remhash request-token (request-tokens server)))

(defmethod revoke-verifier-token ((server simple-server) request-token)
  (remhash request-token (verifier-tokens server)))

(defmethod revoke-access-token ((server simple-server) access-token)
  (remhash access-token (access-tokens server)))

(defmethod record-nonce ((server simple-server) timestamp nonce)
  (push nonce (gethash timestamp (nonces server))))

(defmethod find-nonce ((server simple-server) timestamp nonce)
  (find nonce (gethash timestamp (nonces server)) :test #'equal))

