#|
 This file is a part of north
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem north
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "OAuth 1.0a Server Implementation"
  :homepage "https://github.com/Shinmera/3d-vectors"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "server")
               (:file "documentation"))
  :depends-on (:uuid
               :crypto-shortcuts
               :cl-ppcre))
