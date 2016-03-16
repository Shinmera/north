#|
 This file is a part of north
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem north-example
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An example illustrating the use of North."
  :homepage "https://github.com/Shinmera/north"
  :serial T
  :components ((:file "example"))
  :depends-on (:north
               :drakma
               :hunchentoot
               :jsown
               :clip))
