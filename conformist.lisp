(defpackage :conformist
  (:use :common-lisp))

(in-package :conformist)

(defvar *placeholders* (make-hash-table :test #'equal))

(defun define-placeholder (placeholder recognition-predicate)
  (if (gethash placeholder *placeholders*)
      (format t "You try to redefine an existing placeholder~%")
      (setf (gethash placeholder *placeholders*) recognition-predicate)))

(defun remove-placeholder (placeholder)
  (if (gethash placeholder *placeholders*)
      (remhash placeholder *placeholders*)
      (format t "You try to remove unexisting placeholder~%")))

(defun redefine-placeholder (placeholder recognition-predicate)
  (if (gethash placeholder *placeholders*)
      (setf (gethash placeholder *placeholders*) recognition-predicate)
      (format t "You try to redefine unexisting placeholder~%")))

(defun get-placeholder-predicate (placeholder)
  (values (gethash placeholder *placeholders*) placeholder))

(defun placeholderp (data)
  (if (gethash data *placeholders*)
      t
      nil))

(defun does-placeholder-matches-data (placeholder data)
  (funcall (get-placeholder-predicate placeholder) data))

(in-package :conformist)

(defun does-a-matches-b (a b)
  (if (placeholderp a)
      (does-placeholder-matches-data a b)
    (equal a b)))

(defun matchp-not-safe (pattern data)
  (if (null pattern)
      t
      (let ((pattern-elm (car pattern))
            (data-elm (car data)))
       (if (listp pattern-elm)
           (and (matchp-not-safe pattern-elm data-elm)
                (matchp-not-safe (cdr pattern) (cdr data)))
           (and (does-a-matches-b pattern-elm data-elm)
                (matchp-not-safe (cdr pattern) (cdr data)))))))

(defun matchp (pattern data)
  (if (= (length pattern)
         (length data))
      (matchp-not-safe pattern data)
      nil))
