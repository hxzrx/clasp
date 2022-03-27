(defpackage #:ninja
  (:use #:common-lisp)
  (:export #:wrapping-stream
           #:write-bindings
           #:write-build
           #:write-comment
           #:write-default
           #:write-include
           #:write-pool
           #:write-rule))

(defpackage #:configure
  (:use #:common-lisp)
  (:nicknames #:c)
  (:export #:configure
           #:includes
           #:library
           #:make-source
           #:make-source-output
           #:recurse
           #:source-path
           #:sources))

