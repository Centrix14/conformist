(create-hash-table conformist-collection nil
                   (:symbol (symbolp t))
                   (:list (listp t))
                   (:symbols (symbolp nil))
                   (:lists (listp nil)))

(defun placeholderp (data pattern-system)
  (with-slots ((collection placeholders-collection)) pattern-system
    (multiple-value-bind (value exists) (gethash data collection)
      (declare (ignore value))
      exists)))

(defun does-placeholder-matches-data (placeholder data pattern-system)
  (with-slots ((collection placeholders-collection)) pattern-system
    (funcall (first (gethash placeholder collection)) data)))

(defun does-a-matches-b (a b pattern-system)
  (with-slots (placeholder-predicate data-to-placeholder-matcher) pattern-system
      (if (funcall placeholder-predicate a pattern-system)
          (funcall data-to-placeholder-matcher a b pattern-system)
          (equalp a b))))

(defun match-complex-placeholder (data index edge-placeholder pattern-system)
  (with-slots ((matcher single-values-matcher)) pattern-system
    (loop for i from index to (frontier data)
          while (not
                 (funcall matcher edge-placeholder (elt data i) pattern-system))
          collect (elt data i))))
