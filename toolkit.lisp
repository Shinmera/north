#|
 This file is a part of north
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.north)

(defun pget (key alist)
  (cdr (assoc key alist :test #'string-equal)))

(define-setf-expander pget (key alist)
  (let ((cell (gensym "CELL"))
        (alistv (gensym "ALIST"))
        (value (gensym "VAL")))
    (values (list alistv cell)
            (list alist `(assoc ,key ,alistv :test #'string-equal))
            (list value)
            `(if ,cell
                 (setf (cdr ,cell) ,value)
                 (push (cons ,key ,value) ,alist))
            `(pget ,key ,alistv))))

(defun remove-param (key alist)
  (remove key alist :key #'car :test #'string-equal))

(defun url-encode (thing)
  (with-output-to-string (out)
    (loop for octet across (cryptos:to-octets thing)
          for char = (code-char octet)
          do (cond ((or (char<= #\0 char #\9)
                        (char<= #\a char #\z)
                        (char<= #\A char #\Z)
                        (find char "-._~" :test #'char=))
                    (write-char char out))
                   (T (format out "%~2,'0x" (char-code char)))))))

(defun url-decode (string)
  (let ((out (make-array (length string) :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (loop for i from 0 below (length string)
          for char = (aref string i)
          do (case char
               (#\% (vector-push (parse-integer string :start (+ i 1) :end (+ i 3) :radix 16) out)
                (incf i 2))
               (#\+ (vector-push (char-code #\Space) out))
               (T (vector-push (char-code char) out)))
          finally (return (cryptos:to-string out)))))

(defgeneric sign (method data consumer-secret &optional token-secret)
  (:method ((method (eql :plaintext)) data consumer-secret &optional token-secret)
    (format NIL "~a&~@[~a~]" consumer-secret token-secret))
  (:method ((method (eql :hmac-sha1)) data consumer-secret &optional token-secret)
    (cryptos:hmac data (sign :plaintext NIL consumer-secret token-secret) :digest :sha1))
  (:method ((method (eql :cmac-aes)) data consumer-secret &optional token-secret)
    (cryptos:cmac data (sign :plaintext NIL consumer-secret token-secret) :normalize-key :hash))
  (:method ((method string) data consumer-secret &optional token-secret)
    (sign (find-symbol (string-upcase method) :keyword) data consumer-secret token-secret)))

(defun make-nonce ()
  (write-to-string (uuid:make-v4-uuid)))

(defun make-timestamp ()
  (write-to-string (- (get-universal-time)
                      (encode-universal-time 0 0 0 1 1 1970 0))))

(defun param< (a b)
  (destructuring-bind (akey . aval) a
    (destructuring-bind (bkey . bval) b
      (or (and (string= akey bkey)
               (string< aval bval))
          (string< akey bkey)))))

(defun sort-params (params)
  (sort (copy-list params) #'param<))

(defun concat-params (params &optional (delim "&"))
  (let ((params (sort-params params)))
    (with-output-to-string (out)
      (loop for (pair . rest) on params
            for (key . val) = pair
            do (format out (format NIL "~~a=~~a~~@[~a~~*~~]" delim)
                       (url-encode (etypecase key
                                     (string key)
                                     (symbol (string-downcase key))))
                       (url-encode val) rest)))))

(defun url-parts (url)
  (cl-ppcre:register-groups-bind (scheme host NIL port path)
      ("^([a-zA-Z][a-zA-Z0-9+-.]+)://([a-zA-Z0-9-._:~%!$&'/\\(\\)*+,;=]+?)(:([0-9]+))?/([^?#]*).*$" url)
    (values scheme host port path)))

(defun normalize-url (url)
  (multiple-value-bind (scheme host port path) (url-parts url)
    (format NIL "~(~a~)://~(~a~)~:[:~a~;~*~]/~a"
            scheme host (or (and (string= port "80") (string-equal scheme "HTTP"))
                            (and (string= port "443") (string-equal scheme "HTTPS"))) port path)))

(defun normalize-token (method url params)
  (format NIL "~:@(~a~)&~a&~a"
          method (url-encode (normalize-url url)) (url-encode (concat-params params))))

(defun create-signature (consumer-secret token-secret method url oauth-params &optional get-params)
  (let ((oauth-params (remove-param :oauth_signature oauth-params)))
    (sign (pget :oauth_signature_method oauth-params)
          (normalize-token method url (append oauth-params get-params))
          consumer-secret token-secret)))

(defun destructure-oauth-header (header)
  (unless (and (<= 5 (length header)) (string= "OAuth" header :end2 5))
    (error "Header malformed, couldn't find \"OAuth\" prefix."))
  (let ((stream (make-string-output-stream))
        (key NIL)
        (values ()))
    (flet ((stream-data ()
             (url-decode (get-output-stream-string stream))))
      (loop for i from 6 below (length header)
            for c = (aref header i)
            do (cond ((find c '(#\Space #\Linefeed #\Return #\Tab)))
                     ((char= c #\,)
                      (unless key (error "Header malformed, value without a key."))
                      (push (cons key (stream-data)) values)
                      (setf key NIL))
                     ((char= c #\=)
                      (when key (error "Header malformed, unencoded =."))
                      (setf key (stream-data)))
                     (T
                      (write-char c stream)))
            finally (when key (push (cons key (stream-data)) values))))
    (nreverse values)))

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
