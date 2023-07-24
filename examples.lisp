(in-package :conformist-examples)

(defun skip-one (data index)
  (declare (ignore data))
  (1+ index))

(defun skip-symbols (data index)
  (format t "index: ~a~%" index)
  (let ((elm (elt data index)))
    (loop while (< index (length data)) do
      (unless (symbolp elm)
        (return-from skip-symbols index))
      (setf elm (elt data index))
      (incf index)))
  (format t "skip: ~a~%" (1- index))
  (1- index))

(defun add-placeholders ()
  (map nil #'define-placeholder
       (list :symbol :list :symbols)
       (list #'symbolp #'listp #'symbolp)
       (list #'skip-one #'skip-one #'skip-symbols)))

(defun remove-placeholders ()
  (maphash (lambda (key value)
             (declare (ignore value))
             (remhash key *placeholders*))
           *placeholders*))

(defun test1 ()
  (values
   ;; :list placeholder describes list
   (matchp '(:list) '((1 2 3)))

   ;; :symbol placeholder describes one symbol
   (matchp '(:symbol) '(a))

   ;; placeholders may be nested
   (matchp '(:symbol (:symbol :list)) '(a (b (c d))))

   ;; you can mix placeholders and values
   (matchp '(a :symbol (b :list c)) '(a / (b (1 2 3) c)))))

;; :symbols placeholder describes one or more symbols
(defun test2 ()
  (matchp '(a :symbols) '(a b c d)))

(defun make-tests ()
  (add-placeholders)
  (test1)
  )
