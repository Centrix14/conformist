(defparameter +not-matches+ 'nm)

(defmacro create-hash-table (name make-args &rest values)
  `(progn
     (defvar ,name (make-hash-table ,@make-args))

     (loop for pair in ',values do
       (setf (gethash (first pair) ,name) (second pair)))))

(create-hash-table conformist-collection nil
                   (:symbol (symbolp t))
                   (:list (listp t))
                   (:symbols (symbolp nil))
                   (:lists (listp nil)))

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

(defun placeholderp (data &optional (collection conformist-collection))
  (multiple-value-bind (value exists) (gethash data collection)
    (declare (ignore value))
    exists))

(defun does-placeholder-matches-data (placeholder
                                      data
                                      &optional (collection
                                                 conformist-collection))
  (funcall (first (gethash placeholder collection)) data))

(defvar conformist-scales (list #'placeholderp
                                #'does-placeholder-matches-data
                                #'equal))

(defun does-a-matches-b (a b &optional (scales conformist-scales))
  (destructuring-bind (placeholder-predicate
                       placeholder-matching-predicate
                       equal-predicate)
      scales
    (if (funcall placeholder-predicate a)
        (funcall placeholder-matching-predicate a b)
        (funcall equal-predicate a b))))


(defvar conformist-matching-kit (list #'does-a-matches-b
                                      conformist-scales))

(defun simplep (data &optional (collection conformist-collection))
  (second (gethash data collection)))

(defun match-complex-placeholder (data index edge-placeholder
                                  &optional
                                    (matching-kit conformist-matching-kit))
  (destructuring-bind (matcher scales) matching-kit
    (loop for i from index to (frontier data)
          while (not
                 (funcall matcher edge-placeholder (elt data i) scales))
          collect (elt data i))))

(defvar conformist-lore (list #'simplep
                              #'match-complex-placeholder
                              conformist-matching-kit))

(defun eros (pattern data &optional (lore conformist-lore))
  (destructuring-bind (simple-predicate complex-matcher
                       matching-kit)
      lore
    (let ((pattern-index 0)
          (data-index 0)

          (edge (min (length pattern)
                     (length data)))

          (matcher (first matching-kit))
          (placeholder-predicate (car (car (cdr matching-kit))))
          
          groups)

      (loop while (< pattern-index edge) do
        (let ((wish (elt pattern pattern-index))
              (reality (elt data data-index)))

          (if (and (listp wish)
                   (listp reality))
              (unless (eros wish reality)
                (return-from eros +not-matches+))
              (unless (funcall matcher wish reality)
                (return-from eros +not-matches+)))

          (when (funcall placeholder-predicate wish)
            (when (funcall simple-predicate wish)
              (push reality groups)
              (incf data-index))
            (unless (funcall simple-predicate wish)
              (let ((matched-group
                      (funcall complex-matcher
                               data
                               data-index
                               (elm pattern
                                    (1+ pattern-index))
                               matching-kit)))
                (push matched-group groups)
                (incf data-index (length matched-group)))))
          (unless (funcall placeholder-predicate wish)
            (push reality groups)
            (incf data-index)))
          
        (incf pattern-index))
      groups)))

(defun matchp (pattern data)
  (when (< (length data)
           (length pattern))
    (return-from matchp nil))
  
  (let ((raw (grouping-matcher pattern data)))
    (if (eql raw +not-matches+)
        nil
        (let* ((result (reverse raw))
               (collected (unite result)))
          (if (or (equal result data)
                  (equal collected data))
              (values t result)
              nil)))))

;; TODO: Переделать систему ошибок
(defun test (test-forms)
  (map 'list
       (lambda (test-form)
         (let ((wish (first test-form))
               (reality (eval (second test-form))))
           (if (equalp wish reality)
               t
               (format t "Fail: ~s => ~s~%" test-form
                       (eval (second test-form))))))
       test-forms))

(format t "~a~%"
        (test '((t (matchp '(:list) '((1 2 3))))
                (t (matchp '(:symbol) '(a)))
                (t (matchp '(:lists) '((1 2 3) (4 5 6) (7 8 9))))
                (t (matchp '(:symbols) '(a b c d)))
                (t (matchp '(:symbols :lists) '(a b c (1 2 3) (4 5 6))))
                (t (matchp '(:lists :symbols) '((1 2 3) (4 5 6) a b c)))
                (t (matchp '(:symbol (:symbol)) '(a (b))))
                (t (matchp '(:symbol (:symbols) :list) '(a (b c d) (e f g h))))
        
                (nil (matchp '(:list) '(a)))
                (nil (matchp '(:symbol) '((1 2 3))))
                (nil (matchp '(:lists) '(a b c)))
                (nil (matchp '(:symbols) '((1 2 3) (4 5 6))))
                (nil (matchp '(:symbols :lists) '((1 2 3) a b c)))
                (nil (matchp '(:lists :symbols) '(a b c (1 2 3))))
                (nil (matchp '(:symbol (:symbol)) '(a b)))
                (nil (matchp '(:symbol (:symbols) :list) '(a (b c d) e))))))
