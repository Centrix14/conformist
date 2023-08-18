(in-package :conformist)

(defun compositor (pattern data pattern-system)
  (with-slots ((complex-matcher complex-placeholder-matcher)
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
                (return-from compositor 'not-matches))
              (unless (funcall matcher wish reality pattern-system)
                (return-from compositor 'not-matches)))

          (when (funcall placeholder-predicate wish pattern-system)
            (when (simplep wish pattern-system)
              (push reality groups)
              (incf data-index))
            (unless (simplep wish pattern-system)
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
