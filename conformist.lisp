(defpackage :conformist
  (:use :common-lisp)
  (:export :matchp
           :placeholderp
           :define-placeholder
           :redefine-placeholder
           :remove-placeholder
           :get-recognition-predicate
           :get-shift-function
           :*placeholders*))

(defpackage :conformist-examples
  (:use :common-lisp :conformist))
