(defpackage :conformist
  (:use :common-lisp))

(in-package :conformist)

(defvar *placeholders* (list :symbol :list))

(defun placeholderp (data)
  (member data *placeholders*))

(defun does-placeholder-matches-data (placeholder data)
  (cond
    ((eql placeholder :symbol)
     (symbolp data))
    ((eql placeholder :list)
     (listp data))))

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
