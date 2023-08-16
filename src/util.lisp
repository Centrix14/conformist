(defmacro create-hash-table (name make-args &rest values)
  `(progn
     (defvar ,name (make-hash-table ,@make-args))

     (loop for pair in ',values do
       (setf (gethash (first pair) ,name) (second pair)))))
