(in-package :signal)

(defun sliding-variance (input result window-size)
  "A classic two-pass algorithm to compute the sliding window variance with WINDOW-SIZE on
INPUT into RESULT, both double-float arrays. Returns RESULT. "
  (declare (type (simple-array double-float (*)) input result)
           (type fixnum window-size)
           (optimize (speed 3) (safety 1)))

  (let* ((N (length input))
         (half (floor window-size 2))
         (mean 0.0d0)
         (sum 1.0d0)
         (sum-sq 1.0d0)
         (1/N 0.0d0) ;; (/ 1.0d0 window-size)
         (1/N-1 0.0d0)) ;; (/ 1.0d0 (1- window-size))
    (declare (type double-float mean sum sum-sq 1/N 1/N-1))

    (loop for i from 0 below N        ; case: i=0, half=11 w=23
          for i0 = (max 0 (- i half)) ; i0 = 0
          for ilim = (min (+ i half) (1- n))
          do (progn
               (setf sum 0.0d0)
               (setf sum-sq 0.0d0)
               ;; Fix the constants 
               (cond ((< i half) ; adjust the constants for the prefix part
                      (setf 1/N (/ 1.0d0 (+ i half 1)) ; 1/N = 1/12 for i=0
                            1/N-1 (/ 1.0d0 (+ i half)))) ; 1/N-1 = 1/11
                     ((>= i (- N half))
                      (setf 1/N (/ 1.0d0 (- (+ N half) i))
                            1/N-1 (/ 1.0d0 (- (+ N half -1) i))))
                     ((= i half) ; only have to do set this region once
                      (setf 1/N (/ 1.0d0 (coerce window-size 'double-float)))
                      (setf 1/N-1 (/ 1.0d0 (1- window-size)))))

               ;; Pass 1
               (loop for j from i0 upto ilim
                     do (incf sum (aref input j)))
               (setf mean (* sum 1/N)) ; naive sum

               ;; Pass 2
               (loop for j from i0 upto ilim
                     for v = (- (aref input j) mean)
                     do (incf sum-sq (* v v))) ; naive sum

               (setf (aref result i) (* sum-sq 1/N-1))))
    result))

;; ―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
;; Version using pairwise-summation
;; 
;; - Uses the same number of adds but arranged so double-float sum rounding errors go
;; up as log(n*epsilon) rather n*epsilon,
;; - But at the cost of a lot of extra function calls.
;; - I did some testing and found it typically gives up to an order of magnitude
;; improvement in rounding errors. 

(defun sliding-variance-pw (input result window-size) ; My algorithm + pairwise-summation
  "A classic two-pass algorithm to compute the sliding window variance with WINDOW-SIZE on
INPUT into RESULT, both double-float arrays. Returns RESULT. "
  (declare (type (simple-array double-float (*)) input result)
           (type fixnum window-size)
           (optimize (speed 3) (safety 1)))

  (let* ((N (length input))
         (half (floor window-size 2))
         (mean 0.0d0)
         (sum 1.0d0)
         (sum-sq 1.0d0)
         (1/N 0.0d0)    ;; (/ 1.0d0 window-size)
         (1/N-1 0.0d0)) ;; (/ 1.0d0 (1- window-size))
    (declare (type double-float mean sum sum-sq 1/N 1/N-1))

    (labels ((pairwise-sum (i1 i2)      ; sum input from i1 to i2 inclusive
               (declare (type fixnum i1 i2))
               (cond ((= i1 i2)
                      (the double-float (aref input i1))) ; tell compiler the return type
                     ((= (1+ i1) i2)
                      (the double-float (+ (aref input i1) (aref input i2))))
                     (t 
                      (the double-float
                           (+ (pairwise-sum i1 (+ i1 (floor (- i2 i1) 2)))
                              (pairwise-sum (+ i1 (floor (- i2 i1) 2) 1) i2))))))

             (pairwise-sum-2 (i1 i2)    ; sum input from i1 to i2 inclusive
               (declare (type fixnum i1 i2))
               (if (= i1 i2)                           ;; length == 1
                   (let ((v (- (aref input i1) mean))) ; subtract the mean
                     (declare (type double-float v))
                     (the double-float
                          (* v v)))     ; return it's square
                   (the double-float
                        (+ (pairwise-sum-2 i1 (+ i1 (floor (- i2 i1) 2)))
                           (pairwise-sum-2 (+ i1 (floor (- i2 i1) 2) 1) i2))))))

      (loop for i from 0 below N        ; case: i=0, half=11 w=23
            for i0 = (max 0 (- i half)) ; i0 = 0
            for ilim = (min (+ i half) (1- n))
            do (progn
                 (setf sum 0.0d0)
                 (setf sum-sq 0.0d0)
                 ;; Fix the constants 
                 (cond ((< i half)      ; adjust the constants for the prefix part
                        (setf 1/N (/ 1.0d0 (+ i half 1))   ; 1/N = 1/12 for i=0
                              1/N-1 (/ 1.0d0 (+ i half)))) ; 1/N-1 = 1/11
                       ((>= i (- N half))
                        (setf 1/N (/ 1.0d0 (- (+ N half) i))
                              1/N-1 (/ 1.0d0 (- (+ N half -1) i))))
                       ((= i half)      ; only have to do set this region once
                        (setf 1/N (/ 1.0d0 (coerce window-size 'double-float)))
                        (setf 1/N-1 (/ 1.0d0 (1- window-size)))))

                 ;; Pass 1
                 (setf sum (pairwise-sum i0 ilim))
                 (setf mean (* sum 1/N)) ; naive sum here - should use pairwise-summation

                 ;; Pass 2
                 (setf sum-sq (pairwise-sum-2 i0 ilim))
                 (setf (aref result i) (* sum-sq 1/N-1)) ; not right for end regions - needs to shrink
                 )))
    result))

;;; ――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
;;; This version has prefix section issues that are yet unresolved. 
#+nil
(defun sliding-variance! (input result window-size) 
  "An incremental-shift algorithm to compute the sliding window variance of WINDOW-SIZE on
INPUT into RESULT, both double-float arrays. Returns RESULT. "
  (declare (type (simple-array double-float (*)) input result)
           (type fixnum window-size)
           (optimize (speed 3) (safety 1)))

  (let* ((n (length input))
         (half (floor window-size 2))
         (sum 0.0d0)
         (sum-sq 0.0d0)
         (w-inv (/ 1.0d0 window-size))
         (denom-inv (/ 1.0d0 (1- window-size))))
    (declare (type double-float sum sum-sq w-inv denom-inv))

    ;; 1. Initialize sums for the first window
    (dotimes (i window-size)
      (let ((val (aref input i)))
        (incf sum val)
        (incf sum-sq (* val val))))

    ;; 2. Main loop (Sliding)
    (loop for i from half below (- n half) do
      ;; Calculate variance from sums: (SumSq - (Sum^2 / W)) / (W - 1)
      (let ((var (* (- sum-sq (* (* sum sum) w-inv)) denom-inv)))
        (setf (aref result i) (if (< var 0.0d0) ;; Clamp tiny precision errors to 0
                                  0.0d0 
                                  var))

      ;; Update sums for next iteration
      (when (< (1+ i) (- n half))
        (let ((old-val (aref input (- i half)))
              (new-val (aref input (+ i half 1))))
          (incf sum (- new-val old-val))
          (incf sum-sq (- (* new-val new-val) (* old-val old-val))))))
    result))

#+nil ; Compare test
(let* ((N 2048)
       (input (make-array N :element-type 'double-float 
                            :initial-contents (loop repeat 2048 collect (random 1.0d0))))
      (i0 0)
      (ilim 22)
      (sum 0.0d0))
  ;; Naive summation
  (loop for j from i0 upto ilim do (incf sum (aref input j)))
  (print sum)
  ;; 
  (labels ((pairwise-sum (i1 i2)        ; sum input from i1 to i2 inclusive
             (if (= i1 i2)              ;; length == 1
                 (aref input i1)        ; return it
                 (+ (pairwise-sum i1 (+ i1 (floor (- i2 i1) 2)))
                    (pairwise-sum (+ i1 (floor (- i2 i1) 2) 1) i2)))))
    (print (pairwise-sum i0 ilim)))))

;; ―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
;; 
(defun sliding-variance-filter (in-list wkv)
  "Processes a list of simple double-float arrays with a sliding-variance filter with window
width WKV. Returns a similar list with result. Non-destructive. "
  (loop for vec in in-list
        for frame from 0
        for result = (make-array (length vec) :element-type 'double-float :initial-element 0.0d0)
        collect (sliding-variance vec result wkv)))
