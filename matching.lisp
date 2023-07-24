(in-package :conformist)

(defun does-a-matches-b (a b)
  (format t "~a ~a~%" a b)
  (if (placeholderp a)
      (does-placeholder-matches-data a b)
    (equal a b)))

(defun matchp-unsafe (pattern data)
  (let ((pattern-index 0)
        (data-index 0)
        (pattern-len (length pattern))
        (data-len (length data)))
    (loop while (and (< pattern-index pattern-len)
                     (< data-index data-len))
          do
             (let ((pattern-elm (elt pattern pattern-index))
                   (data-elm (elt data data-index)))

               (if (listp pattern-elm)
                   (unless (matchp-unsafe pattern-elm data-elm)
                     (return-from matchp-unsafe nil))
                   (unless (does-a-matches-b pattern-elm data-elm)
                     (return-from matchp-unsafe nil)))

               (if (placeholderp pattern-elm)
                   (setf data-index (funcall (get-shift-function pattern-elm)
                                               data
                                               data-index))
                   (incf data-index))
               (incf pattern-index)))
    t))

(defun matchp (pattern data)
  (if (and (listp pattern)
           (listp data))
      (matchp-unsafe pattern data)))
