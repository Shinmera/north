(asdf:defsystem north-drakma
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Drakma backend for North"
  :homepage "https://Shinmera.github.io/north/"
  :bug-tracker "https://github.com/Shinmera/north/issues"
  :source-control (:git "https://github.com/Shinmera/north.git")
  :serial T
  :components ((:file "drakma"))
  :depends-on (:drakma
               :north-core))
