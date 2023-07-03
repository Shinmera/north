(in-package #:org.shirakumo.north)

(defmethod call ((request request) &rest drakma-args)
  (let ((drakma:*text-content-types*
          (list* '("application" . "x-www-form-urlencoded")
                 drakma:*text-content-types*)))
    (multiple-value-bind (body status-code headers)
        (apply #'drakma:http-request
               (url request)
               :method (http-method request)
               :parameters (parameters request)
               :additional-headers (headers request)
               :url-encoder #'url-encode
               :external-format-in *external-format*
               :external-format-out *external-format*
               drakma-args)
      (unless (<= 200 status-code 299)
        (error 'request-failed :request request :body body :status-code status-code :headers headers))
      body)))
