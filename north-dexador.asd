#|
 This file is a part of north
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem north-dexador
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Dexador backend for North"
  :homepage "https://github.com/Shinmera/north"
  :serial T
  :components ((:file "dexador"))
  :depends-on (:dexador
               :north-core))
