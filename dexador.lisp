#|
 This file is a part of north
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.north)

(defun parameters->string (params)
  (with-output-to-string (out)
    (loop for cons on params
          for param = (car cons)
          do (format out "~a=~a" (url-encode (car param)) (url-encode (cdr param)))
             (when (cdr cons) (format out "&")))))

(defun convert-received-headers (headers)
  (loop for k being the hash-keys of headers
        for v being the hash-values of headers
        collect (cons (intern (string-upcase k) :keyword) v)))

(defun purify-form-data (params)
  (loop for param in params
        collect (if (listp (cdr param))
                    (cons (car param) (second param))
                    param)))

(defmethod call ((request request) &rest dexador-args &key form-data)
  (let ((params (quri:make-uri :query (parameters->string (parameters request))))
        (uri (quri:uri (url request))))
    (multiple-value-bind (body status-code headers uri)
        (handler-bind ((dexador:http-request-failed (lambda (err)
                                                      (invoke-restart 'dexador:ignore-and-continue))))
          (apply #'dexador:request
                 (if form-data uri (quri:merge-uris params uri))
                 :method (http-method request)
                 :headers (headers request)
                 :content (when form-data (purify-form-data (parameters request)))
                 (loop for (key val) on dexador-args by #'cddr
                       unless (eql key :form-data) collect key
                       unless (eql key :form-data) collect val)))
      (unless (<= 200 status-code 299)
        (error 'request-failed :request request :body body :status-code status-code :headers headers))
      body)))

