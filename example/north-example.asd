(asdf:defsystem north-example
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An example illustrating the use of North."
  :homepage "https://Shinmera.github.io/north/"
  :bug-tracker "https://github.com/Shinmera/north/issues"
  :source-control (:git "https://github.com/Shinmera/north.git")
  :serial T
  :components ((:file "package")
               (:file "server")
               (:file "client"))
  :depends-on (:north
               :drakma
               :hunchentoot
               :clip))
