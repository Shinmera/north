#|
 This file is a part of north
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.north)

(defgeneric create-request-token-pair (server))
(defgeneric create-access-token-pair (server))
(defgeneric consumer-secret (server consumer-key))
(defgeneric token-secret (server token))
(defgeneric oauth/request-token (server request))
(defgeneric oauth/authorize (server request))
(defgeneric oauth/access-token (server request))
(defgeneric oauth/verify (server request))

(defclass server ()
  ((nonces :initform (make-hash-table :test 'equal) :accessor nonces)))

(defmethod create-request-token-pair ((server server))
  (values (make-nonce) (make-nonce)))

(defmethod create-access-token-pair ((server server))
  (values (make-nonce) (make-nonce)))

(defun check-parameters-present (request &rest parameters)
  (loop for param in parameters
        unless (pget param (oauth request))
        collect param into missing
        finally ;; FIXME
        (when missing (error "Parameters ~s missing!" missing))))

(defun check-request (request server)
  (let ((consumer-key (pget :oauth_consumer_key (oauth request)))
        (token-secret (pget :oauth_token (oauth request)))
        (version (pget :oauth_version (oauth request)))
        (nonce (pget :oauth_nonce (oauth request)))
        (timestamp (pget :oauth_timestamp (oauth request))))
    (if (find timestamp (gethash nonce (nonces server)) :test #'string=)
        ;; FIXME
        (error "Nonce reused.")
        (push timestamp (gethash nonce (nonces server))))
    (unless (or (not version) (string= "1.0" version))
      ;; FIXME
      (error "Bad version."))
    (unless (verify request
                    (consumer-secret server consumer-key)
                    (token-secret server token))
      ;; FIXME
      (error "Signature failed!"))))

(defmethod oauth/request-token ((server server) (request request))
  (check-parameters-present request
    :oauth_consumer_key :oauth_signature_method :oauth_signature
    :oauth_timestamp :oauth_nonce :oauth_callback)
  (check-request request server)
  (multiple-value-bind (request-token token-secret) (create-request-token-pair server)
    (values request-token token-secret T)))

(defmethod oauth/authorize ((server server) (request request))
  (check-parameters-present request :oauth_token)
  (values token verifier))

(defmethod oauth/access-token ((server server) (request request))
  (check-parameters-present request
    :oauth_consumer_key :oauth_signature_method :oauth_signature
    :oauth_timestamp :oauth_nonce :oauth_token :oauth-verifier)
  (check-request request server)
  (values token token-secret))

(defmethod oauth/verify ((server server) (request request))
  (check-parameters-present request
    :oauth_consumer_key :oauth_signature_method :oauth_signature
    :oauth_timestamp :oauth_nonce :oauth_token)
  (check-request request server)
  T)
