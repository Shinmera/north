#|
 This file is a part of north
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem north-dexador
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Dexador backend for North"
  :homepage "https://Shinmera.github.io/north/"
  :bug-tracker "https://github.com/Shinmera/north/issues"
  :source-control (:git "https://github.com/Shinmera/north.git")
  :serial T
  :components ((:file "dexador"))
  :depends-on (:dexador
               :north-core))
