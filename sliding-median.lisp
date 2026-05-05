(in-package :signal)

;;; ――――――――――――――――――――――――――――――――――――――――――――――――――――――――
;; (declaim (optimize (speed 3) (safety 0) (debug 0)))
;; (declaim (optimize (speed 3) (safety 1) (debug 1)))
;; Applies to everything compiled below.
;; Safety 0:
;; ⟶ doesn't check for errors which can corrupt memory or segfault
;; ⟶ test with (safety 1) until run comprehensively.
;; ⟶ better used locally, inside a function

;; This version focuses on minimizing allocations and maximizing the use of SBCL's
;; type-specialized array operations.

;;; Initial brute-force algorithm
#+nil
(defun sliding-sort-median (x w)
  "Runs a sliding median on array of double-float's X with window width 2*(floor w)+1. "
  (let* ((w (1+ (* 2 (floor w 2))))     ; ensure w is odd. 
         (xlen (length x))
         (y (make-array xlen :element-type 'double-float))   ; result array
         (win (make-array w :element-type 'double-float ))   ; window array 
         (wl (floor w 2))
         (wr (- w wl)))
    (loop for i from 0 below xlen
          for il = (max 0 (- i wl))
          for ir = (min xlen (+ i wr))
          do (progn 
               (cond ((= (- ir il) w)   ; full-width window to sort
                      (loop for j from il below ir
                            for k from 0
                            do (setf (aref win k) (aref x j)))
                      (let ((win* (sort win #'<)))
                        ;; (push (copy-list (coerce win* 'list)) *winl*) ; DEBUGGING
                        (setf (aref y i) (aref win* wl))))
                     ;; End effects are a problem. Can't sort on partially empty arrays
                     (t
                      (loop for j from il below ir
                            for k from 0
                            collect (aref x j) into l
                            finally (let ((sl (sort l #'<))) ;; use list sort with exact-length lists
                                      (push (copy-list sl) *winl*)
                                      (setf (aref y i) (nth (floor (1- (length sl)) 2) sl))))))))
    (values y)))

;;; ――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
;;; Sliding Median Filter - hybrid
;;; ⟶ focus on speed and minimizing GC activity
(declaim (inline update-sorted-buffer!)) ;; stop boxing this buffer

(defun update-sorted-buffer! (sorted old-val new-val w)
  "Helper function for sliding-medians"
  (declare (type (simple-array double-float (*)) sorted)
           (type double-float old-val new-val)
           (type fixnum w)
           ;; (optimize (speed 3) (safety 1) (debug 1))
           (optimize (speed 3) (safety 0) (debug 0))) 

  (let ((old-pos (position old-val sorted :test #'=)))
    (when old-pos
      (if (< new-val old-val)
          (let ((new-pos (loop for j from (1- old-pos) downto 0
                               if (<= (aref sorted j) new-val) return (1+ j)
                                 finally (return 0))))
            (replace sorted sorted :start1 (1+ new-pos) :end1 (1+ old-pos) 
                                   :start2 new-pos :end2 old-pos)
            (setf (aref sorted new-pos) new-val))
          (let ((new-pos (loop for j from (1+ old-pos) below w
                               if (>= (aref sorted j) new-val) return (1- j)
                                 finally (return (1- w)))))
            (replace sorted sorted :start1 old-pos :end1 new-pos 
                                   :start2 (1+ old-pos) :end2 (1+ new-pos))
            (setf (aref sorted new-pos) new-val))))))

;; ―――――――――――――――――――――――――――――――――――――――――――――――――――――――
#+nil ;; Hybrid Version 1

(defun sliding-median-hybrid.1 (input window-size)
  (declare (type (simple-array double-float (*)) input)
           (type (integer 3 501) window-size)
           ;; (optimize (speed 3) (safety 1) (debug 1))
           (optimize (speed 3) (safety 0) (debug 0))) ; once it's safe 
  (let* ((n (length input))
         (result (make-array n :element-type 'double-float))
         (half (floor window-size 2))
         ;; The buffer for the sliding interior
         (sorted (make-array window-size :element-type 'double-float)))
    (declare (type (simple-array double-float (*)) result sorted)
             (type fixnum n half))

    ;; --- 1. Prefix (Growing Window) ---
    ;; Use part of current-w for s scratch area to sort up until :end1
    (dotimes (i half)
      (let* ((current-w (min n (+ i half 1)))
             ;; Create a temporary view of the first 'current-w' elements
             (view (make-array current-w 
                               :element-type 'double-float 
                               :displaced-to sorted)))
        (declare (type (vector double-float) view))
        ;; Copy input into the scratchpad
        (replace sorted input :end1 current-w :end2 current-w)
        ;; Sort the view (which sorts the first part of 'sorted' in-place)
        (setf view (sort view #'<))
        (setf (aref result i) (aref view (floor (1- current-w) 2)))))

    ;; --- 2. Interior (Sliding Window) ---
    ;; Initialize the sliding buffer with the window centered at index 'half'
    ;; This window covers indices [0 ... window-size-1]
    (replace sorted input :end1 window-size :end2 window-size)
    (setf sorted (sort sorted #'<))
    
    (loop for i from half below (- n half) do
      (setf (aref result i) (aref sorted (floor window-size 2)))
      ;; Update for the NEXT iteration
      (when (< (1+ i) (- n half))
        (let ((old-val (aref input (- i half)))
              (new-val (aref input (+ i half 1))))
          (unless (= old-val new-val)
            (update-sorted-buffer! sorted old-val new-val window-size)))))

    ;; --- 3. Suffix (Shrinking Window) ---
    (loop for i from (- n half) below n do
      (let* ((start (max 0 (- i half)))
             ;; current-w is the number of samples from start to the very end
             (current-w (- n start))
             (view (make-array current-w 
                               :element-type 'double-float 
                               :displaced-to sorted)))
        (declare (type (vector double-float) view))
        ;; 1. Copy the shrinking window from the input tail into our scratchpad
        (replace sorted input :end1 current-w :start2 start :end2 n)
        ;; 2. Sort only the 'view' (the first current-w elements of sorted)
        (setf view (sort view #'<))
        ;; 3. Pick the median using the same "lower median" logic
        (setf (aref result i) (aref view (floor (1- current-w) 2)))))

    result))

;; ―――――――――――――――――――――――――――――――――――――――――――――――――――――――
;; Sliding-Median-Filter (hybrid Version 2)

;; Takes care of buffer array allocation,
(defun sliding-median (input window-size result sorted)
  "Runs sliding median filter of WINDOW-SIZE on double-float array INPUT and returns
double-float RESULT. NOTE: window-size is limited to 501 bytes to keep an internal sorting
buffer in fast memory."
  (declare (type (simple-array double-float (*)) input result sorted)
           (type (integer 3 501) window-size)
           ;; (optimize (speed 3) (safety 1) (debug 1))
           (optimize (speed 3) (safety 0) (debug 0))) ; once it's safe 
  (let* ((n (length input))
         (half (floor window-size 2)))
    (declare (type fixnum n half))

    ;; --- 1. Prefix (Growing Window) ---
    ;; Use part of current-w for s scratch area to sort up until :end1
    (dotimes (i half)
      (let* ((current-w (min n (+ i half 1)))
             ;; Create a temporary view of the first 'current-w' elements
             (view (make-array current-w 
                               :element-type 'double-float 
                               :displaced-to sorted)))
        (declare (type (vector double-float) view))
        ;; Copy input into the scratchpad
        (replace sorted input :end1 current-w :end2 current-w)
        ;; Sort the view (which sorts the first part of 'sorted' in-place)
        (setf view (sort view #'<))
        (setf (aref result i) (aref view (floor (1- current-w) 2)))))

    ;; --- 2. Interior (Sliding Window) ---
    ;; Initialize the sliding buffer with the window centered at index 'half'
    ;; This window covers indices [0 ... window-size-1]
    (replace sorted input :end1 window-size :end2 window-size)
    (setf sorted (sort sorted #'<))
    
    (loop for i from half below (- n half) do
      (setf (aref result i) (aref sorted (floor window-size 2)))
      ;; Update for the NEXT iteration
      (when (< (1+ i) (- n half))
        (let ((old-val (aref input (- i half)))
              (new-val (aref input (+ i half 1))))
          (unless (= old-val new-val)
            (update-sorted-buffer! sorted old-val new-val window-size)))))

    ;; --- 3. Suffix (Shrinking Window) ---
    (loop for i from (- n half) below n do
      (let* ((start (max 0 (- i half)))
             ;; current-w is the number of samples from start to the very end
             (current-w (- n start))
             (view (make-array current-w 
                               :element-type 'double-float 
                               :displaced-to sorted)))
        (declare (type (vector double-float) view))
        ;; 1. Copy the shrinking window from the input tail into our scratchpad
        (replace sorted input :end1 current-w :start2 start :end2 n)
        ;; 2. Sort only the 'view' (the first current-w elements of sorted)
        (setf view (sort view #'<))
        ;; 3. Pick the median using the same "lower median" logic
        (setf (aref result i) (aref view (floor (1- current-w) 2)))))

    result))

;; The following version manages it's own result and sorted buffers, but is not
;; thread-safe.
(let ((result (make-array 0 :element-type 'double-float))
      (sorted (make-array 0 :element-type 'double-float)))
  (declare (type (simple-array double-float (*)) result sorted))

  ;; Hybrid version
  (defun sliding-median! (input window-size)
    "Runs sliding median filter on INPUT double-float array of WINDOW-SIZE and returns RESULT array. 
NOTE: Result is a shared resource array. It will change on the next call to this function,
so it needs to be copied off with COPY-SEQ first. NOTE: window-size is limited to 501
bytes to keep sorting window in fast memory. "
    (declare (type (simple-array double-float (*)) input)
             (type (integer 3 501) window-size)
             ;; (optimize (speed 3) (safety 1) (debug 1))
             (optimize (speed 3) (safety 0) (debug 0))) ; once it's safe 

    (let ((n (length input)))
      (unless (= n (length result))
        (setf result (make-array n :element-type 'double-float)))
      (unless (= window-size (length sorted))
        (setf sorted (make-array window-size :element-type 'double-float))))

    (sliding-median input window-size result sorted)))

;;; ――――――――――――――――――――――――――――――――――――――――――――――
;;; Testing
;;; ――――――――
;;; Test data for verifying the hybrid sliding median against the brute force version. 

;; (defparameter *data* nil)
#+nil ; generate data
(let* ((n 1000000)
       (l (loop repeat n for x = (random 100.d0) collect (* 0.1d0 (floor x 0.1d0))))
       (input (make-array n :element-type 'double-float :initial-contents l))
       ;;       (fa (fast-median input w))
       ;;(ga (sliding-sort-median input w))
       )
  (print (length (unique l :test #'=)))
  (setf *input* input))

;;; ――――――――――――――――――――――――――――――――――――――――――――――
;;; Compare two of the functions. Stops on first error and
;;; displays the sorted window of the hybrid for debugging. 
#+nil
(let* ((input *input*)
       (n (length input))
       (w 11)
       (half (floor w 2))
       ;; (fa (fast-median input w)) (lbl "Fast-")
       fa ga
       (lbl "Hybrid-")
       ;;(ga (sliding-sort-median input w))
       )
  (time (setf fa (sliding-median input w)))
  (time (setf ga (sliding-sort-median input w)))
  (loop for i from 0 below n
        for f-val = (aref fa i)
        for g-val = (aref ga i)
        unless (= f-val g-val)
          do (let* ((start (max 0 (- i half)))
                    (end   (min n (+ i half 1)))
                    (truth (sort (subseq input start end) #'<)))
               (format t "~%--- Mismatch at Index ~D ---~%" i)
               (format t "~aMedian Output: ~F~%" lbl f-val)
               (format t "Brute-Force Output: ~F~%" g-val)
               (format t "Full sorted window should be:~% ~A~%" truth)
               ;; This is the "Audit" — if this fails, the buffer logic is broken
               (return))
        finally (format t "~%Success! All ~D samples match.~%" n)))

;;; ――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
;;; Performance Notes

;; Getting roughly 3x the speed and 40x reduction in memory allocation using the hybrid.

;; The most interesting part of the benchmark is the GC time: 0.406s of the 0.463s total
;; time was spent on GC. So the actual math and sliding logic is only taking about
;; 0.057s. The rest is Lisp cleaning up the small amount of memory used (likely from the
;; sort and subseq calls in the prefix/suffix logic).

;; SBCL usually does a great job, but it can sometimes struggle to inline the search used
;; in POSITION:

;; 1. Generic vs. Specialized:
;; 
;; POSITION is a generic sequence function. It works on lists, strings, and vectors. It
;; can introduce boxing where it wraps the double-floats into an object on the heap, just
;; to compare it. Or it uses a generic compare function that is slower than a raw machine
;; instruction.

;; 2. The Manual Loop (inline):

;; In the update-sorted-buffer! function, has a manual loop to find new-pos. Because that
;; loop is inside the function and uses type declarations like (type double-float
;; new-val), SBCL's compiler can use the CPU's UCOMISD instruction to compare these two
;; numbers directly."

;; In many languages, high-level functions are always faster because they are written in
;; C. In SBCL Common Lisp, a well-annotated manual loop can outperform the built-in
;; sequence functions because the compiler can optimize the loop specifically for the data
;; type (double-float), whereas the built-in functions have to be prepared for anything.

;; Ensure all loop variables are declared or inferred as fixnum.

;; Eliminate SUBSEQ in the prefix/suffix sections to reduce the 0.4s GC pause. SUBSEQ
;; creates a new array every time it is called. Use a single scratch buffer. 

;;; ―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
;;; 
(defun sliding-median-filter (in-list wkm)
  "Processes a list of simple double-float arrays with a sliding-median filter of window
width WKM. Returns a similar list with result. Non-destructive. "
  (let ((sorted (make-array wkm :element-type 'double-float)))
    (loop for vec in in-list
          for frame from 0
          for result = (make-array (length vec) :element-type 'double-float :initial-element 0.0d0)
          collect (sliding-median vec wkm result sorted))))


