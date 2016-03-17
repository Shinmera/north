#|
 This file is a part of north
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.north.example)

(defvar *server* (make-instance 'north:simple-server))
(defvar *hunchentoot* (make-instance 'hunchentoot:easy-acceptor :port 4242))
(defvar *authorize-page* (asdf:system-relative-pathname :north-example "authorize.ctml"))

(defvar *consumer* (north:make-consumer *server* :name "North"))

(defmacro json-out (&rest stuff)
  `(progn
     (setf (hunchentoot:content-type*) "application/json;charset=utf-8")
     (jsown:to-json
      (jsown:new-js ,@stuff))))

(defun error-out (err)
  (json-out
   ("status" "error")
   ("error_type" (string (type-of err)))
   ("error_text" (let ((*print-escape* NIL)) (write-to-string err)))))

(defmacro with-error-handling (() &body body)
  `(handler-case
       (progn ,@body)
     (north:parameter-error (err)
       (setf (hunchentoot:return-code*) 400)
       (error-out err))
     (north:verification-error (err)
       (setf (hunchentoot:return-code*) 401)
       (error-out err))
     (error (err)
       (setf (hunchentoot:return-code*) 500)
       (error-out err))))

(defun make-request (request)
  (make-instance
   'north:request
   :http-method (hunchentoot:request-method* request)
   :url (hunchentoot:request-uri* request)
   :post-params (hunchentoot:post-parameters* request)
   :get-params (hunchentoot:get-parameters* request)
   :headers (hunchentoot:headers-in* request)))

(hunchentoot:define-easy-handler (oauth/request-token :uri "/oauth/request-token") ()
  (with-error-handling ()
    (multiple-value-bind (token secret callback-confirmed) (north:oauth/request-token *server* (make-request hunchentoot:*request*))
      (json-out
       ("oauth_token" token)
       ("oauth_token_secret" secret)
       ("oauth_callback_confirmed" callback-confirmed)))))

(hunchentoot:define-easy-handler (oauth/authenticate :uri "/oauth/authorize") (oauth_token verifier error)
  (with-error-handling ()
    (let* ((session (or (north:session server oauth_token)
                        (error 'north:invalid-token :request (make-request hunchentoot:*request*))))
           (consumer (north:consumer server (north:key session))))
      (setf (hunchentoot:content-type*) "application/xml+html;charset=utf-8")
      (clip:process *authorize-page*
                    :oauth_token oauth_token
                    :consumer consumer
                    :verifier verifier
                    :error error))))

(hunchentoot:define-easy-handler (oauth/authorize :uri "/oauth/authenticate") (oauth-token action)
  (with-error-handling ()
    (cond ((string= action "Deny")
           "Shiet")
          ((string= action "Allow")
           (multiple-value-bind (token verifier url) (north:oauth/authorize *server* (make-request hunchentoot:*request*))
             (if url
                 (hunchentoot:redirect url)
                 (hunchentoot:redirect (format NIL"/oauth/authorize?oauth_token=~a&verifier=~a"
                                               (north:url-encode token) (north:url-encode verifier))))))
          (T
           (hunchentoot:redirect "/oauth/authorize?error=Invalid%20action.")))))

(hunchentoot:define-easy-handler (oauth/access-token :uri "/oauth/access-token") ()
  (with-error-handling ()
    (multiple-value-bind (token secret) (north:oauth/access-token *server* (make-request hunchentoot:*request*))
      (json-out
       ("oauth_token" token)
       ("oauth_token_secret" secret)))))

(hunchentoot:define-easy-handler (oauth/verify :uri "/oauth/verify") ()
  (with-error-handling ()
    (north:oauth/verify *server* (make-request hunchentoot:*request*))
    (json-out
     ("status" "success"))))
