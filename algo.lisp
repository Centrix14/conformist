(defparameter +not-matches+ 'not-matches)

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

(defun placeholderp (data pattern-system)
  (with-slots ((collection placeholders-collection)) pattern-system
    (multiple-value-bind (value exists) (gethash data collection)
      (declare (ignore value))
      exists)))

(defun simplep (data pattern-system)
  (with-slots ((collection placeholders-collection)) pattern-system
    (second (gethash data collection))))

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

(defclass pattern-system ()
  ((placeholders-collection
    :initarg :placeholders-collection
    :initform conformist-collection
    :documentation "Placeholders collection")
   
   (placeholder-predicate
    :type function
    :initarg :placeholder-predicate
    :initform #'placeholderp
    :documentation "Predicate for placeholder recognition")

   (simple-placeholder-predicate
    :type function
    :initarg :simple-placeholder-predicate
    :initform #'simplep
    :documentation "Predicate that recognize simple placeholders")

   (data-to-placeholder-matcher
    :type function
    :initarg :data-to-placeholder-matcher
    :initform #'does-placeholder-matches-data
    :documentation "Function for matching data to given placeholder")

   (single-values-matcher
    :type function
    :initarg :single-values-matcher
    :initform #'does-a-matches-b
    :documentation "Function for matching two single values")

   (complex-placeholder-matcher
    :type function
    :initarg :complex-placeholder-matcher
    :initform #'match-complex-placeholder
    :documentation "Function for matching data to complex placeholder"))
  
  (:documentation "Pattern system contains placeholder collection and collection-specific functions"))

(defun compositor (pattern data pattern-system)
  (with-slots ((simple-predicate simple-placeholder-predicate)
               (complex-matcher complex-placeholder-matcher)
               (matcher single-values-matcher)
               placeholder-predicate)
      pattern-system
    
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
              (unless (compositor wish reality pattern-system)
                (return-from compositor +not-matches+))
              (unless (funcall matcher wish reality pattern-system)
                (return-from compositor +not-matches+)))

          (when (funcall placeholder-predicate wish pattern-system)
            (when (funcall simple-predicate wish pattern-system)
              (push reality groups)
              (incf data-index))
            (unless (funcall simple-predicate wish pattern-system)
              (let ((matched-group
                      (funcall complex-matcher
                               data
                               data-index
                               (elm pattern
                                    (1+ pattern-index))
                               pattern-system)))
                (push matched-group groups)
                (incf data-index (length matched-group)))))
          
          (unless (funcall placeholder-predicate wish pattern-system)
            (push reality groups)
            (incf data-index)))
          
        (incf pattern-index))
      
      groups)))

(defun shipper (pattern data pattern-system)
  (when (< (length data)
           (length pattern))
    (return-from shipper nil))
  
  (let ((raw (compositor pattern data pattern-system)))
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
        (test '((t (shipper '(:list) '((1 2 3))))
                (t (shipper '(:symbol) '(a)))
                (t (shipper '(:lists) '((1 2 3) (4 5 6) (7 8 9))))
                (t (shipper '(:symbols) '(a b c d)))
                (t (shipper '(:symbols :lists) '(a b c (1 2 3) (4 5 6))))
                (t (shipper '(:lists :symbols) '((1 2 3) (4 5 6) a b c)))
                (t (shipper '(:symbol (:symbol)) '(a (b))))
                (t (shipper '(:symbol (:symbols) :list) '(a (b c d) (e f g h))))
        
                (nil (shipper '(:list) '(a)))
                (nil (shipper '(:symbol) '((1 2 3))))
                (nil (shipper '(:lists) '(a b c)))
                (nil (shipper '(:symbols) '((1 2 3) (4 5 6))))
                (nil (shipper '(:symbols :lists) '((1 2 3) a b c)))
                (nil (shipper '(:lists :symbols) '(a b c (1 2 3))))
                (nil (shipper '(:symbol (:symbol)) '(a b)))
                (nil (shipper '(:symbol (:symbols) :list) '(a (b c d) e))))))
