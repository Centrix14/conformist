(in-package :conformist)

(defun shipper (pattern data pattern-system)
  (when (< (length data)
           (length pattern))
    (return-from shipper nil))

  (let ((raw (compositor pattern data pattern-system)))
    (if (eql raw 'not-matches)
        nil
        (let* ((result (reverse raw))
               (collected (unite result)))
          (if (or (equal result data)
                  (equal collected data))
              (values t result)
              nil)))))
