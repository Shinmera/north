#|
 This file is a part of north
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem north-example
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An example illustrating the use of North."
  :homepage "https://github.com/Shinmera/north"
  :serial T
  :components ((:file "package")
               (:file "server")
               (:file "client"))
  :depends-on (:north
               :drakma
               :hunchentoot
               :clip))
