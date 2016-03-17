#|
 This file is a part of north
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.north)

(defvar *external-format* :utf-8)

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
    (loop for octet across (cryptos:to-octets thing *external-format*)
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

(defun concat-params (params &key quote (delim "&"))
  (let ((params (sort-params params)))
    (with-output-to-string (out)
      (loop for (pair . rest) on params
            for (key . val) = pair
            do (when (and key val)
                 (format out (format NIL "~~a=~:[~~a~;~~s~]~~@[~a~~*~~]" quote delim)
                         (url-encode (etypecase key
                                       (string key)
                                       (symbol (string-downcase key))))
                         (url-encode val) rest))))))

(defun url-parts (url)
  (or (cl-ppcre:register-groups-bind (scheme host NIL port NIL path)
          ("^([a-zA-Z][a-zA-Z0-9+-.]+)://([a-zA-Z0-9-._:~%!$&'\\(\\)*+,;=]+?)(:([0-9]+))?(/+([^?#]*).*)?$" url)
        (list scheme host port (or path "")))
      (error "Invalid URL ~s" url)))

(defun normalize-url (url)
  (destructuring-bind (scheme host port path) (url-parts url)
    (format NIL "~(~a~)://~(~a~)~:[:~a~;~*~]/~a"
            scheme host (or (not port)
                            (and (string= port "80") (string-equal scheme "HTTP"))
                            (and (string= port "443") (string-equal scheme "HTTPS"))) port path)))

(defun normalize-token (method url params)
  (format NIL "~:@(~a~)&~a&~a"
          method (url-encode (normalize-url url)) (url-encode (concat-params params))))

(defun create-signature (consumer-secret token-secret method url oauth-params &optional get-params)
  (let ((oauth-params (remove-param :oauth_signature oauth-params)))
    (sign (pget :oauth_signature_method oauth-params)
          (normalize-token method url (append oauth-params get-params))
          consumer-secret token-secret)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *whitespace* '(#\Space #\Linefeed #\Return #\Tab)))

(defun start-p (start string)
  (and (<= (length start) (length string))
       (string= start string :end2 (length start))))

(defun destructure-oauth-header (header)
  (unless (start-p "OAuth" header)
    (error "Header malformed, couldn't find \"OAuth\" prefix."))
  (when (< (length "OAuth .") (length header))
    (loop with buffer = (make-string-output-stream)
          with input = (make-string-input-stream header 6)
          with values = ()
          with key = NIL
          with state = :expecting-key
          for c = (read-char input NIL NIL) while c
          do (labels ((write-to-buffer ()
                        (write-char c buffer))
                      (buffer-contents ()
                        (url-decode (get-output-stream-string buffer)))
                      (change-state (new-state)
                        (setf state new-state)
                        (case state
                          (:end-key (setf key (buffer-contents)))
                          (:end-val (push (cons key (buffer-contents))
                                          values))))
                      (change-state-immediate (new-state)
                        (unread-char c input) (change-state new-state))
                      (parse-error ()
                        (error "Invalid character ~c in state ~s" c state)))
               (case state
                 (:expecting-key
                  (case c
                    (#.*whitespace* NIL)
                    ((#\, #\= #\")  (parse-error))
                    (T              (change-state-immediate :key))))
                 (:key
                  (case c
                    (#.*whitespace* (change-state :end-key))
                    ((#\=)          (change-state-immediate :end-key))
                    ((#\, #\")      (parse-error))
                    (T              (write-to-buffer))))
                 (:end-key
                  (case c
                    (#.*whitespace* NIL)
                    ((#\=)          (change-state :expecting-val))
                    (T              (parse-error))))
                 (:expecting-val
                  (case c
                    (#.*whitespace* NIL)
                    ((#\")          (change-state :val))
                    (T              (parse-error))))
                 (:val
                  (case c
                    ((#\")          (change-state :end-val))
                    ((#\, #\=)      (parse-error))
                    (T              (write-to-buffer))))
                 (:end-val
                  (case c
                    (#.*whitespace* NIL)
                    ((#\,)          (change-state :expecting-key))
                    (T              (write-to-buffer))))))
          finally (unless (eql state :end-val)
                    (error "Unexpected end in state ~s" state))
                  (return (nreverse values)))))

(defun oauth-response->alist (body)
  (mapcar (lambda (assignment)
            (let ((pair (cl-ppcre:split "=" assignment)))
              (cons (url-decode (first pair))
                    (url-decode (second pair)))))
          (cl-ppcre:split "&" body)))

(defun alist->oauth-response (alist)
  (concat-params alist))
