(in-package :conformist)

(defmacro create-hash-table (make-args &rest values)
  `(let ((tmp (make-hash-table ,@make-args)))
     (loop for pair in ',values do
       (setf (gethash (first pair) tmp) (second pair)))
     tmp))

(defun frontier (sequence)
  (1- (length sequence)))

(defun elm (sequence index)
  (if (> index (frontier sequence))
      nil
      (elt sequence index)))

(defun listify (value)
  (if (listp value)
      value
      (list value)))

(defun unite (sequence)
  (reduce (lambda (x y)
            (append (listify x)
                    (listify y)))
          sequence))

(defun simplep (placeholder pattern-system)
  (with-slots (placeholders-collection) pattern-system
      (second (gethash placeholder placeholders-collection))))
