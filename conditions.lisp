#|
 This file is a part of north
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.north)

(define-condition north-condition (condition)
  ((request :initarg :request :reader request))
  (:default-initargs :request (error "REQUEST required.")))

(define-condition parameter-error (north-condition error)
  ())

(define-condition verification-error (north-condition error)
  ())

(define-condition client-error (north-condition error)
  ())

(define-condition parameters-missing (parameter-error)
  ((parameters :initarg :parameters :reader parameters))
  (:default-initargs :parameters (error "PARAMETERS required."))
  (:report (lambda (c s) (format s "The oauth parameters ~s are required but from the request."
                                 (parameters c)))))

(define-condition bad-version (parameter-error)
  ()
  (:default-initargs :version (error "VERSION required."))
  (:report (lambda (c s) (format s "The version ~s is not supported, must be \"1.0\"."
                                 (pget :oauth_version (oauth (request c)))))))

(define-condition verifier-taken (verification-error)
  ()
  (:report (lambda (c s) (format s "The verifier for the request token ~s has already been generated."
                                 (pget :oauth_token (oauth (request c)))))))

(define-condition nonce-reused (verification-error)
  ()
  (:report (lambda (c s) (format s "The nonce ~s has already been used."
                                 (pget :oauth_nonce (oauth (request c)))))))

(define-condition invalid-signature (verification-error)
  ()
  (:report (lambda (c s) (format s "Unable to verify the signature of the request."))))

(define-condition invalid-verifier (verification-error)
  ()
  (:report (lambda (c s) (format s "The verifier ~s is invalid for this request token."
                                 (pget :oauth_verifier (oauth (request c)))))))

(define-condition invalid-token (verification-error)
  ()
  (:report (lambda (c s) (format s "The token ~s is invalid."
                                 (pget :oauth_token (oauth (request c)))))))

(define-condition invalid-consumer (verification-error)
  ()
  (:report (lambda (c s) (format s "No consumer with key ~s."
                                 (pget :oauth_consumer_key (oauth (request c)))))))

(define-condition request-failed (client-error)
  ((body :initarg :body :reader body)
   (status-code :initarg :status-code :reader status-code)
   (headers :initarg :headers :reader headers))
  (:default-initargs
   :body (error "BODY required.")
   :status-code (error "STATUS-CODE required.")
   :headers (error "HEADERS required."))
  (:report (lambda (c s) (format s "~a Request to ~a failed with status code ~a."
                                 (http-method (request c)) (url (request c)) (status-code c)))))

(define-condition callback-unconfirmed (client-error)
  ()
  (:report (lambda (c s) (format s "Callback was not confirmed on token request."))))
