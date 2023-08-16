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
