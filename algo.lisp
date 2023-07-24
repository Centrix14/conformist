(defparameter +not-matches+ 'nm)

(defun placeholderp (data)
  (if (member data '(:symbol :list :symbols :lists))
      t
      nil))

(defun simplep (pattern)
  (if (member pattern '(:symbols :lists))
      nil
      t))

(defun does-placeholder-matches-data (placeholder data)
  (case placeholder
    ((:symbol :symbols) (symbolp data))
    ((:list :lists) (listp data))
    ))

(defun does-a-matches-b (a b)
  (if (placeholderp a)
      (does-placeholder-matches-data a b)
      (equal a b)))

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

(defun match-complex-placeholder (data index edge-placeholder)
  (loop for i from index to (frontier data)
        while (not (does-a-matches-b edge-placeholder (elt data i)))
        collect (elt data i)))

(defun grouping-matcher (pattern data)
  (let ((pattern-index 0)
        (data-index 0)

        (edge (min (length pattern)
                   (length data)))

        groups)
    
    (loop while (< pattern-index edge) do
      (let ((wish (elt pattern pattern-index))
            (reality (elt data data-index)))

        (if (and (listp wish)
                 (listp reality))
            (unless (grouping-matcher wish reality)
              (return-from grouping-matcher +not-matches+))
            (unless (does-a-matches-b wish reality)
              (return-from grouping-matcher +not-matches+)))

        (when (simplep wish)
          (push reality groups)
          (incf data-index))
        (unless (simplep wish)
          (let ((matched-group
                  (match-complex-placeholder data
                                              data-index
                                              (elm pattern
                                                   (1+ pattern-index)))))
            (push matched-group groups)
            (incf data-index (length matched-group))))
        
        (incf pattern-index)))
    
    groups))

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
