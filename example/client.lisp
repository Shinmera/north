#|
 This file is a part of north
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.north.example)

(defparameter *client* (make-instance 'north:client
                                      :key (north:key *application*)
                                      :secret (north:secret *application*)
                                      :callback "oob"
                                      :request-token-uri "http://localhost:4242/oauth/request-token"
                                      :authorize-uri "http://localhost:4242/oauth/authorize"
                                      :access-token-uri "http://localhost:4242/oauth/access-token"
                                      :verify-uri "http://localhost:4242/oauth/verify"))
