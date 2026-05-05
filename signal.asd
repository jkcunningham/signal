(asdf:defsystem #:signal
  :description "Package wrapping ffmpeg for Common Lisp"
  :author "Jeff Cunningham <jeffrey@jkcunningham.com>"
  :license  "MIT License"
  :version "1.0"
  ;; :depends-on (#:parse-number)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "sliding-variance")
               (:file "sliding-median")
               ))
