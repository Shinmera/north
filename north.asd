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
  :description "oAuth 1.0a server and client implementation, the successor to South."
  :homepage "https://github.com/Shinmera/north"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "conditions")
               (:file "request")
               (:file "server")
               (:file "client")
               (:file "documentation"))
  :depends-on (:uuid
               :crypto-shortcuts
               :cl-ppcre
               :drakma
               :documentation-utils))
