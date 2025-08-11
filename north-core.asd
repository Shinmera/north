(asdf:defsystem north-core
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Core functionality of North, excluding the HTTP backend."
  :homepage "https://shinmera.com/docs/north/"
  :bug-tracker "https://shinmera.com/project/north/issues"
  :source-control (:git "https://shinmera.com/project/north.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "conditions")
               (:file "request")
               (:file "server")
               (:file "client")
               (:file "documentation"))
  :depends-on (:frugal-uuid
               :babel
               :ironclad/mac/hmac
               :ironclad/digest/sha1
               :cl-base64
               :cl-ppcre
               :documentation-utils))
