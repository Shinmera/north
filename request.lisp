#|
 This file is a part of north
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.north)

(defgeneric make-signed (request consumer-secret &optional token-secret))
(defgeneric make-authorized (request))
(defgeneric verify (request consumer-secret &optional token-secret))

(defclass request ()
  ((http-method :initarg :http-method :accessor http-method)
   (url :initarg :url :accessor url)
   (get-params :initarg :get-params :accessor get-params)
   (post-params :initarg :post-params :accessor post-params)
   (headers :initarg :headers :accessor headers)
   (oauth :initarg :oauth :accessor oauth))
  (:default-initargs
   :http-method :GET
   :url "http://example.com"
   :get-params ()
   :post-params ()
   :headers ()
   :oauth ()))

(defmethod initialize-instance :after ((request request) &key)
  (macrolet ((default (key val)
               `(or (pget ,key (oauth request))
                    (setf (pget ,key (oauth request)) ,val))))
    (default :oauth_nonce (make-nonce))
    (default :oauth_signature_method "HMAC-SHA1")
    (default :oauth_timestamp (make-timestamp))
    (default :oauth_version "1.0"))
  (when (pget "Authorization" (headers request))
    (setf (oauth request) (destructure-oauth-header (pget "Authorization" (headers request))))))

(defmethod make-signed ((request request) consumer-secret &optional token-secret)
  (setf (pget :oauth_signature (oauth request))
        (create-signature consumer-secret token-secret (http-method request)
                          (url request) (oauth request) (get-params request)))
  request)

(defmethod make-authorized ((request request))
  (unless (pget :oauth_signature (oauth request))
    (error "Request ~a must be signed first!" request))
  (setf (pget "Authorization" (headers request))
        (format NIL "OAuth ~a" (concat-params (oauth request) ", ")))
  request)

(defmethod verify ((request request) consumer-secret &optional token-secret)
  (let* ((oauths (destructure-oauth-header (pget "Authorization" (headers request))))
         (old-sig (pget :oauth_signature oauths))
         (new-sig (create-signature consumer-secret token-secret (http-method request)
                                    (url request) (oauth request) (get-params request))))
    
    (string= old-sig new-sig)))
