(asdf:defsystem north-dexador
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Dexador backend for North"
  :homepage "https://shinmera.com/docs/north/"
  :bug-tracker "https://shinmera.com/project/north/issues"
  :source-control (:git "https://shinmera.com/project/north.git")
  :serial T
  :components ((:file "dexador"))
  :depends-on (:dexador
               :north-core))
