(asdf:defsystem #:configure
  :description "A lisp system to configure the CLASP build."
  :depends-on (#:alexandria
               #:clasp-scraper
               #:shasht
               #:trivial-features
               #:trivial-gray-streams)
  :components ((:file "packages")
               (:file "utilities")
               (:file "ninja")
               (:file "header")
               (:file "source")
               (:file "configure")
               (:file "target-sources")
               (:file "units")
               (:file "outputs")))
