(defpackage #:signal
  (:use #:cl)
  (:export
   ;; ――――――――――――――――――― util.lisp
   #:todb
   ;; ――――――――――――――――――― sliding-variance.lisp
   #:sliding-median
   #:sliding-median!
   #:sliding-variance-filter
   #:sliding-variance
   #:sliding-variance-pw
   #:sliding-median-filter
   ))
