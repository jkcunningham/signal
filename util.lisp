(in-package :signal)

(defun todb (ar &optional (minval 1.0d-12))
  "Does an in-place conversion of array elements to 10*log10 and returns the array. "
  (loop for n from 0 below (length ar)
        for x = (aref ar n)
        do (setf (aref ar n) (if (<= x 0) minval (* 10 (log x 10)))))
  (values ar))

