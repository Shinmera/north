(in-package #:org.shirakumo.north.example)

(defparameter *client* (make-instance 'north:client
                                      :key (north:key *application*)
                                      :secret (north:secret *application*)
                                      :callback "oob"
                                      :request-token-uri "http://localhost:4242/oauth/request-token"
                                      :authorize-uri "http://localhost:4242/oauth/authorize"
                                      :access-token-uri "http://localhost:4242/oauth/access-token"
                                      :verify-uri "http://localhost:4242/oauth/verify"))
