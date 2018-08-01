
(defmethod staple:packages ((system (eql (asdf:find-system :north))))
  (mapcar #'find-package '(:north)))

(defmethod staple:subsystems ((system (eql (asdf:find-system :north))))
  (mapcar #'asdf:find-system '(:north-drakma :north-dexador)))
