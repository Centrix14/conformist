(in-package :conformist)

(defvar *placeholders* (make-hash-table :test #'equal))

(defun define-placeholder (placeholder recognition-predicate shift-function)
  (if (gethash placeholder *placeholders*)
      (format t "You try to redefine an existing placeholder~%")
      (setf (gethash placeholder *placeholders*) (list recognition-predicate
                                                       shift-function))))

(defun remove-placeholder (placeholder)
  (if (gethash placeholder *placeholders*)
      (remhash placeholder *placeholders*)
      (format t "You try to remove unexisting placeholder~%")))

(defun redefine-placeholder (placeholder recognition-predicate shift-function)
  (if (gethash placeholder *placeholders*)
      (setf (gethash placeholder *placeholders*) (list recognition-predicate
                                                       shift-function))
      (format t "You try to redefine unexisting placeholder~%")))

(defun get-recognition-predicate (placeholder)
  (values (first (gethash placeholder *placeholders*)) placeholder))

(defun get-shift-function (placeholder)
  (values (second (gethash placeholder *placeholders*)) placeholder))

(defun placeholderp (data)
  (if (gethash data *placeholders*)
      t
      nil))

(defun does-placeholder-matches-data (placeholder data)
  (funcall (get-recognition-predicate placeholder) data))
