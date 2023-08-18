(defmacro create-hash-table (make-args &rest values)
  `(let ((tmp (make-hash-table ,@make-args)))
     (loop for pair in ',values do
       (setf (gethash (first pair) tmp) (second pair)))
     tmp))
