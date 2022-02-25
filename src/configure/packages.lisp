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
  (:export #:sources
           #:includes
           #:recurse
           #:add-output
           #:library
           #:source-path
           #:make-source
           #:make-source-output
           #:configure))

