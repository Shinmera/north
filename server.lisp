#|
 This file is a part of north
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.north)

(defvar *request-tokens* (make-hash-table :test 'equal))
(defvar *access-tokens* (make-hash-table :test 'equal))
(defvar *nonces* (make-hash-table :test 'equal))

;; (defun request-token (consumer-key signature-method signature timestamp nonce &key (version "1.0") (callback "oob"))
;;   (values token token-secret callback-confirmed))

;; (defun authorize (token token-secret)
;;   (values token verifier))

;; (defun access-token (consumer-key token signature-method signature timestamp nonce verifier &key (version "1.0"))
;;   (values token token-secret))

;; (defun verify (consumer-key token signature-method signature timestamp nonce &key (version "1.0"))
;;   )
