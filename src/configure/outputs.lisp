(in-package #:configure)

(defparameter +os-features+ '(:bsd :darwin :freebsd :linux :unix))

(defun print-asdf-stub (output-stream host &rest systems)
  (pprint '(require :asdf) output-stream)
  (pprint `(asdf:initialize-source-registry
             (list :source-registry
                (list :tree (merge-pathnames ,*code-path* (uiop:getcwd)))
                :inherit-configuration))
           output-stream)
  (pprint `(asdf:initialize-output-translations
             (list :output-translations
                (list t (list (merge-pathnames ,(if host
                                                    (make-pathname :directory '(:relative "host-fasl"))
                                                    (merge-pathnames (make-pathname :directory '(:relative "fasl"))
                                                                     *variant-path*))
                                               (uiop:getcwd))
                              :implementation))
                :inherit-configuration))
           output-stream)
  (loop for system in systems
        do (pprint `(asdf:load-system ,system) output-stream)))

(defmethod print-prologue (configuration (name (eql :generate-sif)) output-stream)
  (declare (ignore configuration))
  (print-asdf-stub output-stream t :clasp-scraper)
  (pprint '(apply #'uiop:symbol-call "CSCRAPE" "GENERATE-SIF" (uiop:command-line-arguments)) output-stream))

(defmethod print-prologue (configuration (name (eql :generate-headers)) output-stream)
  (declare (ignore configuration))
  (print-asdf-stub output-stream t :clasp-scraper)
  (with-standard-io-syntax (pprint `(destructuring-bind (cl-user::precise cl-user::variant-path &rest cl-user::args)
               (uiop:command-line-arguments)
             (apply #'uiop:symbol-call "CSCRAPE" "GENERATE-HEADERS"
                    (equal "1" cl-user::precise)
                    cl-user::variant-path
                    ,*code-path*
                    cl-user::args))
           output-stream)))

(defmethod print-prologue (configuration (name (eql :compile-module)) output-stream)
  (format output-stream
          "(compile-file (merge-pathnames (elt core:*command-line-arguments* 1)
                               (ext:getcwd))
              :output-file (merge-pathnames (elt core:*command-line-arguments* 0)
                                            (ext:getcwd))
              :output-type ~s)"
          (case (build-mode configuration)
            (:faso :fasp)
            (:fasoll :faspll)
            (:fasobc :faspbc)
            (otherwise :fasl))))

(defmethod print-prologue (configuration (name (eql :compile-aclasp)) output-stream)
  (format output-stream "(setq *features* (cons :aclasp *features*))
(load #P\"sys:kernel;clasp-builder.lisp\")
(setq core::*number-of-jobs* ~a)
(core:compile-aclasp)
(core:quit)" (jobs configuration)))

(defmethod print-prologue (configuration (name (eql :compile-bclasp)) output-stream)
  (format output-stream "(setq *features* (cons :bclasp *features*))
(load #P\"sys:kernel;clasp-builder.lisp\")
(setq core::*number-of-jobs* ~a)
(core:compile-bclasp)
(core:quit)" (jobs configuration)))

(defmethod print-prologue (configuration (name (eql :compile-cclasp)) output-stream)
  (format output-stream "(setq *features* (cons :cclasp *features*))
(load #P\"sys:kernel;clasp-builder.lisp\")
(setq core::*number-of-jobs* ~a)
(core:compile-cclasp)
(core:quit)" (jobs configuration)))

(defmethod print-prologue (configuration (name (eql :link-fasl)) output-stream)
  (write-string "(setq *features* (cons :aclasp *features*))
(load #P\"sys:kernel;clasp-builder.lisp\")
(load #P\"sys:kernel;cmp;jit-setup.lisp\")
(let ((args (core:command-line-arguments-as-list)))
  (core:link-fasl :output-file (car args)
                  :system (cdr args)))
(core:quit)" output-stream))

(defmethod print-prologue (configuration (name (eql :static-analyzer)) output-stream)
  (declare (ignore configuration))
  (print-asdf-stub output-stream nil :clasp-analyzer)
  (format output-stream "~%(clasp-analyzer::serial-search/generate-code
  (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
    (pathname (elt core:*command-line-arguments* 1)))
 :output-file (pathname (elt core:*command-line-arguments* 0)))"))

(defmethod make-output-stream (configuration (name (eql :ninja)) path)
  (declare (ignore configuration name))
  (make-instance 'ninja::wrapping-stream
                 :stream (open path :direction :output :if-exists :supersede
                               :if-does-not-exist :create)))

(defmethod print-prologue (configuration (name (eql :ninja)) output-stream)
  (ninja:write-bindings output-stream
                        :cflags (cflags configuration)
                        :cppflags (cppflags configuration)
                        :cxxflags (cxxflags configuration)
                        :ar (ar configuration)
                        :cc (cc configuration)
                        :cxx (cxx configuration)
                        :ldflags (ldflags configuration)
                        :ldlibs (ldlibs configuration)
                        :lisp "sbcl" ;(first (uiop:raw-command-line-arguments))
                        :ldflags_fasl (if (uiop:os-macosx-p)
                                          "-flat_namespace -undefined dynamic_lookup -bundle"
                                          "-shared"))
  (terpri output-stream)
  (ninja:write-rule output-stream :copy-file
                    :command "cp -f $in $out"
                    :description "Copying $in to $out")
  (ninja:write-rule output-stream :scrape-pp
                    :command "$cxx $variant-cppflags $cppflags -MD -MF $out.d -o$out -E -DSCRAPING $in"
                    :description "Preprocess $in for scrapping"
                    :depfile "$out.d")
  (ninja:write-rule output-stream :generate-sif
                    :command "$lisp --script generate-sif.lisp $out $in"
                    :description "Scrapping $in")
  (ninja:write-rule output-stream :generate-headers
                    :command "$lisp --script generate-headers.lisp $precise $variant-path $in"
                    :description "Creating headers from sif files")
  (ninja:write-rule output-stream :static-analyzer
                    :command "$iclasp --non-interactive --feature ignore-extensions --load ${variant-path}static-analyzer.lisp -- $sif $in"
                    :description "Analyzing clasp"
                    :pool "console")
  (ninja:write-rule output-stream :ar
                    :command "$ar ru $out $in"
                    :description "Creating archive for $in")
  (ninja:write-rule output-stream :cc
                    :command "$cc $variant-cflags $cflags -c -MD -MF $out.d -o$out $in"
                    :description "Compiling $in"
                    :depfile "$out.d")
  (ninja:write-rule output-stream :cxx
                    :command "$cxx $variant-cxxflags $cxxflags -c -MD -MF $out.d -o$out $in"
                    :description "Compiling $in"
                    :depfile "$out.d")
  (ninja:write-rule output-stream :link
                    :command "$cxx $variant-ldflags $ldflags -o$out $in $variant-ldlibs $ldlibs"
                    :description "Linking $out")
  (ninja:write-rule output-stream :bc
                    :command "$cxx $variant-cxxflags $cxxflags -emit-llvm -c -MD -MF $out.d -o$out $in"
                    :description "Creating bitcode for $in"
                    :depfile "$out.d")
  (ninja:write-rule output-stream :compile-aclasp
                    :command "$iclasp --norc --type image --disable-mpi --ignore-image --feature clasp-min --load compile-aclasp.lisp -- $source"
                    :description "Compiling aclasp"
                    :pool "console")
  (ninja:write-rule output-stream :compile-bclasp
                    :command "$iclasp --norc --type image --disable-mpi --image $image --load compile-bclasp.lisp -- $source"
                    :description "Compiling bclasp"
                    :pool "console")
  (ninja:write-rule output-stream :compile-cclasp
                    :command "$iclasp --norc --type image --disable-mpi --image $image --load compile-cclasp.lisp -- $source"
                    :description "Compiling cclasp"
                    :pool "console")
  (ninja:write-rule output-stream :compile-module
                    :command "$iclasp --non-interactive --norc --type image --disable-mpi --image $image --feature ignore-extensions --load compile-module.lisp -- $out $in"
                    :description "Compiling module $in")
  (ninja:write-rule output-stream :regression-tests
                    :command "$iclasp --non-interactive --feature ignore-extensions --load \"sys:regression-tests;run-all.lisp\""
                    :description "Running regression tests"
                    :pool "console")
  (ninja:write-rule output-stream :link-fasl
                    :command "$iclasp --norc --type image --disable-mpi --ignore-image --feature clasp-min --load link-fasl.lisp -- $out $in"
                    :description "Linking $target")
  (ninja:write-rule output-stream "link-fasl-abc"
                    :command "$cxx $variant-ldflags $ldflags $ldflags-fasl -o$out $in $variant-ldlibs $ldlibs"
                    :description "Linking $out"))

(defmethod print-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :install-code)) source
     &aux (output (make-source (source-path source) :install-clasp)))
  (declare (ignore configuration))
  (ninja:write-build output-stream :copy-file
                     :explicit-inputs (list source)
                     :explicit-outputs (list output))
  (list :outputs output))

(defmethod print-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :install-code)) sources
     &key outputs &allow-other-keys)
  (ninja:write-build output-stream :phony
                     :explicit-inputs outputs
                     :explicit-outputs (list (build-name "install_code" :common t))))

(defmethod print-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :extension-load)) source
     &aux (output (make-source (format nil "extension-startup-loads/~a"
                                       (file-namestring (source-path source)))
                               :install-bin)))
  (declare (ignore configuration))
  (ninja:write-build output-stream :copy-file
                     :explicit-inputs (list source)
                     :explicit-outputs (list output))
  (list :outputs output))

(defmethod print-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :extension-load)) sources
     &key outputs &allow-other-keys)
  (declare (ignore configuration sources))
  (ninja:write-build output-stream :phony
                     :explicit-inputs outputs
                     :explicit-outputs (list (build-name "install_load" :common t))))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :extension-load)) source
     &aux (output (make-source (format nil "extension-startup-loads/~a"
                                       (file-namestring (source-path source)))
                               :variant)))
  (declare (ignore configuration))
  (ninja:write-build output-stream :copy-file
                     :explicit-inputs (list source)
                     :explicit-outputs (list output))
  (list :loads output))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :extension-load)) sources
     &key loads &allow-other-keys)
  (ninja:write-build output-stream :phony
                     :explicit-inputs loads
                     :explicit-outputs (list (build-name "load"))))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :iclasp))
     (source c-source))
  (declare (ignore configuration))
  (let ((o (make-source-output source :type "o")))
    (ninja:write-build output-stream :cc
                       :variant-cflags *variant-cflags*
                       :explicit-inputs (list source)
                       :order-only-inputs (if *variant-precise*
                                              (scraper-precise-headers configuration)
                                              (scraper-headers configuration))
                       :explicit-outputs (list o))
    (list :objects o)))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :iclasp))
     (source cc-source))
  (let ((pp (make-source-output source :type "pp"))
        (sif (make-source-output source :type "sif"))
        (o (make-source-output source :type "o"))
        (flags (format nil "-I~a" *variant-path*)))
    (ninja:write-build output-stream :scrape-pp
                       :variant-cppflags *variant-cppflags*
                       :explicit-inputs (list source)
                       :explicit-outputs (list pp))
    (ninja:write-build output-stream :generate-sif
                       :explicit-inputs (list pp)
                       :explicit-outputs (list sif))
    (ninja:write-build output-stream :cxx
                       :variant-cxxflags *variant-cxxflags*
                       :explicit-inputs (list source)
                       :order-only-inputs (if *variant-precise*
                                              (scraper-precise-headers configuration)
                                              (scraper-headers configuration))
                       :explicit-outputs (list o))
    (list :objects o
          :sifs sif)))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :iclasp))
     (source sif-source))
  (declare (ignore configuration name output-stream target))
  (list :sifs source))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :iclasp)) sources
     &key objects sifs &allow-other-keys
     &aux (generated (append (if *variant-precise*
                                 (scraper-precise-headers configuration)
                                 (scraper-headers configuration))
                             (scraper-lisp-sources configuration)))
          (outputs (list (make-source (build-name target) :variant))))
  (ninja:write-build output-stream :generate-headers
                     :explicit-inputs sifs
                     :precise (if *variant-precise* "1" "0")
                     :variant-path *variant-path*
                     :sources (format nil "~{~a~^ ~}"
                                      (mapcar #'source-path sifs))
                     :explicit-outputs generated)
  (ninja:write-build output-stream :phony
                     :explicit-outputs (list (build-name "generated"))
                     :explicit-inputs generated)
  (ninja:write-build output-stream :link
                     :variant-ldflags *variant-ldflags*
                     :variant-ldlibs *variant-ldlibs*
                     :explicit-inputs objects
                     :explicit-outputs outputs)
  (ninja:write-build output-stream :phony
                     :explicit-inputs outputs
                     :explicit-outputs (list (build-name target))))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream
     (target (eql :bitcode)) (source cc-source)
     &aux (bitcode-name (format nil "fasl/~a-~a-cxx.bc" *variant-bitcode-name*
                                (pathname-name (source-path source))))
          (bitcode-output (make-source bitcode-name :variant))
          (bitcode-install (make-source bitcode-name :install-variant))
          (bitcode-nd-name (format nil "fasl/~a-~a-no-debug-info-cxx.bc" *variant-bitcode-name*
                                   (pathname-name (source-path source))))
          (bitcode-nd-output (make-source bitcode-nd-name :variant))
          (bitcode-nd-install (make-source bitcode-nd-name :install-variant))
          (object (make-source-output source :type "o"))
          (archive-name (format nil "fasl/~a-~a-cxx.a" *variant-bitcode-name*
                                (pathname-name (source-path source))))
          (archive-output (make-source archive-name :variant))
          (archive-install (make-source archive-name :install-variant))
          (object-nd (make-source (make-pathname :directory (pathname-directory (source-path source))
                                                 :name (concatenate 'string
                                                                    (pathname-name (source-path source))
                                                                    "-no-debug-info")
                                                 :type "o")
                                  :variant))
          (archive-nd-name (format nil "fasl/~a-~a-no-debug-info-cxx.a" *variant-bitcode-name*
                                   (pathname-name (source-path source))))
          (archive-nd-output (make-source archive-nd-name :variant))
          (archive-nd-install (make-source archive-nd-name :install-variant))
          (headers (if *variant-precise*
                       (scraper-precise-headers configuration)
                       (scraper-headers configuration)))
          (no-debug-flags (remove-flag "-g" *variant-cxxflags*)))
  (ninja:write-build output-stream :bc
                     :variant-cxxflags *variant-cxxflags*
                     :explicit-inputs (list source)
                     :order-only-inputs headers
                     :explicit-outputs (list bitcode-output))
  (ninja:write-build output-stream :copy-file
                     :explicit-inputs (list bitcode-output)
                     :explicit-outputs (list bitcode-install))
  (ninja:write-build output-stream :bc
                     :variant-cxxflags no-debug-flags
                     :explicit-inputs (list source)
                     :order-only-inputs headers
                     :explicit-outputs (list bitcode-nd-output))
  (ninja:write-build output-stream :copy-file
                     :explicit-inputs (list bitcode-nd-output)
                     :explicit-outputs (list bitcode-nd-install))
  (ninja:write-build output-stream :ar
                     :explicit-inputs (list object)
                     :explicit-outputs (list archive-output))
  (ninja:write-build output-stream :copy-file
                     :explicit-inputs (list archive-output)
                     :explicit-outputs (list archive-install))
  (ninja:write-build output-stream :cxx
                     :variant-cxxflags no-debug-flags
                     :explicit-inputs (list source)
                     :order-only-inputs headers
                     :explicit-outputs (list object-nd))
  (ninja:write-build output-stream :ar
                     :explicit-inputs (list object-nd)
                     :explicit-outputs (list archive-nd-output))
  (ninja:write-build output-stream :copy-file
                     :explicit-inputs (list archive-nd-output)
                     :explicit-outputs (list archive-nd-install))
  (list :install-outputs (list bitcode-install bitcode-nd-install
                               archive-install archive-nd-install)
        :outputs (list bitcode-output bitcode-nd-output
                       archive-output archive-nd-output)))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream
     (target (eql :bitcode)) sources
     &key install-outputs outputs &allow-other-keys)
  (ninja:write-build output-stream :phony
                     :explicit-inputs outputs
                     :explicit-outputs (list (build-name "bitcode")))
  (ninja:write-build output-stream :phony
                     :explicit-inputs install-outputs
                     :explicit-outputs (list (build-name "install_bitcode"))))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream
     (target (eql :aclasp)) sources
     &key &allow-other-keys)
  (let ((aimage (image-source configuration :aclasp))
        (iclasp (make-source (build-name :iclasp) :variant)))
    (ninja:write-build output-stream :compile-aclasp
                       :iclasp iclasp
                       :source (format nil "~{~a~^ ~}" (mapcar (lambda (source)
                                                                 (merge-pathnames (make-pathname :type :unspecific)
                                                                                  (source-path source)))
                                                               sources))
                       :explicit-inputs sources
                       :implicit-inputs (list iclasp
                                              (build-name "bitcode"))
                       :explicit-outputs (make-source-outputs sources
                                                              :type (file-faso-extension configuration)
                                                              :root (format nil "fasl/aclasp-~a-bitcode/"
                                                                            *variant-bitcode-name*)))
    (ninja:write-build output-stream (case (build-mode configuration)
                                       ((:faso :fasoll :fasobc) :link-fasl)
                                       (otherwise "link-fasl-abc"))
                       :variant-ldflags *variant-ldflags*
                       :variant-ldlibs *variant-ldlibs*
                       :iclasp iclasp
                       :target "aclasp"
                       :explicit-inputs (make-source-outputs sources :type (file-faso-extension configuration)
                                                             :root (format nil "fasl/aclasp-~a-bitcode/"
                                                                                *variant-bitcode-name*))
                       :implicit-inputs (list iclasp)
                       :explicit-outputs (list aimage))
    (ninja:write-build output-stream :phony
                       :explicit-inputs (list aimage)
                       :explicit-outputs (list (build-name :aclasp)))
    (ninja:write-build output-stream :copy-file
                       :explicit-inputs (list aimage)
                       :explicit-outputs (list (image-source configuration :aclasp :install-variant)))))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :bclasp)) sources
     &key &allow-other-keys)
  (let ((aimage (image-source configuration :aclasp))
        (bimage (image-source configuration :bclasp))
        (iclasp (make-source (build-name :iclasp) :variant)))
    (ninja:write-build output-stream :compile-bclasp
                       :iclasp iclasp
                       :image aimage
                       :source (format nil "~{~a~^ ~}" (mapcar (lambda (source)
                                                                 (merge-pathnames (make-pathname :type :unspecific)
                                                                                  (source-path source)))
                                                               sources))
                       :explicit-inputs sources
                       :implicit-inputs (list iclasp aimage)
                       :explicit-outputs (make-source-outputs sources
                                                              :type (file-faso-extension configuration)
                                                              :root (format nil "fasl/bclasp-~a-bitcode/"
                                                                            *variant-bitcode-name*)))
    (ninja:write-build output-stream (case (build-mode configuration)
                                       ((:faso :fasoll :fasobc) :link-fasl)
                                       (otherwise "link-fasl-abc"))
                       :variant-ldflags *variant-ldflags*
                       :variant-ldlibs *variant-ldlibs*
                       :iclasp iclasp
                       :target "bclasp"
                       :explicit-inputs (make-source-outputs sources :type (file-faso-extension configuration)
                                                             :root (format nil "fasl/bclasp-~a-bitcode/"
                                                                                *variant-bitcode-name*))
                       :implicit-inputs (list (make-source (build-name :iclasp) :variant))
                       :explicit-outputs (list bimage))
    (ninja:write-build output-stream :phony
                       :explicit-inputs (list bimage)
                       :explicit-outputs (list (build-name :bclasp)))
    (ninja:write-build output-stream :copy-file
                       :explicit-inputs (list bimage)
                       :explicit-outputs (list (image-source configuration :bclasp :install-variant)))))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :cclasp)) sources
     &key &allow-other-keys)
  (let ((bimage (image-source configuration :bclasp))
        (cimage (image-source configuration :cclasp))
        (iclasp (make-source (build-name :iclasp) :variant)))
    (ninja:write-build output-stream :compile-cclasp
                       :iclasp iclasp
                       :image bimage
                       :source (format nil "~{~a~^ ~}" (mapcar (lambda (source)
                                                                 (merge-pathnames (make-pathname :type :unspecific)
                                                                                  (source-path source)))
                                                               sources))
                       :explicit-inputs sources
                       :implicit-inputs (list iclasp bimage)
                       :explicit-outputs (make-source-outputs sources
                                                              :type (file-faso-extension configuration)
                                                              :root (format nil "fasl/cclasp-~a-bitcode/"
                                                                            *variant-bitcode-name*)))
    (ninja:write-build output-stream (case (build-mode configuration)
                                       ((:faso :fasoll :fasobc) :link-fasl)
                                       (otherwise "link-fasl-abc"))
                       :variant-ldflags *variant-ldflags*
                       :variant-ldlibs *variant-ldlibs*
                       :iclasp iclasp
                       :target "cclasp"
                       :explicit-inputs (make-source-outputs sources :type (file-faso-extension configuration)
                                                             :root (format nil "fasl/cclasp-~a-bitcode/"
                                                                                *variant-bitcode-name*))
                       :implicit-inputs (list iclasp)
                       :explicit-outputs (list cimage))
    (ninja:write-build output-stream :copy-file
                       :explicit-inputs (list cimage)
                       :explicit-outputs (list (image-source configuration :cclasp :install-variant)))))

(defmethod print-variant-target-source
    (configuration (name (eql :ninja)) output-stream (target (eql :modules)) (source lisp-source))
  (let* ((image (image-source configuration :cclasp))
         (name (pathname-name (source-path source)))
         (module-name (format nil "fasl/cclasp-~a-bitcode/src/lisp/modules/~a/~a.~a"
                              *variant-bitcode-name* name name
                              (module-fasl-extension configuration)))
         (output (make-source module-name :variant))
         (install-output (make-source module-name :install-variant))
         (iclasp (make-source (build-name :iclasp) :variant)))
    (ninja:write-build output-stream :compile-module
                       :iclasp iclasp
                       :image image
                       :explicit-inputs (list source)
                       :implicit-inputs (list iclasp image)
                       :explicit-outputs (list output))
    (ninja:write-build output-stream :copy-file
                       :explicit-inputs (list output)
                       :explicit-outputs (list install-output))
    (list :outputs output
          :install-outputs install-output)))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :modules)) sources
     &key outputs install-outputs &allow-other-keys
     &aux (executable (build-name :cclasp)))
  (ninja:write-build output-stream :phony
                     :explicit-inputs outputs
                     :explicit-outputs (list (build-name "modules")))
  (ninja:write-build output-stream :phony
                     :explicit-inputs install-outputs
                     :explicit-outputs (list (build-name "install_modules"))))

(defmethod print-variant-target-sources
    (configuration (name (eql :ninja)) output-stream (target (eql :clasp)) sources
     &key &allow-other-keys
     &aux (executable (build-name :cclasp))
          (iclasp (make-source (build-name :iclasp) :variant))
          (inst-iclasp (make-source (build-name :iclasp) :install-bin))
          (generated (append (if *variant-precise*
                                 (scraper-precise-headers configuration)
                                 (scraper-headers configuration))
                             (scraper-lisp-sources configuration)))
          (products (mapcar (lambda (source)
                              (make-source (source-path source) :install-variant))
                            generated)))
  (ninja:write-build output-stream :phony
                     :explicit-inputs (list (build-name "modules")
                                            (build-name "load"))
                     :explicit-outputs (list executable))
  (ninja:write-build output-stream :regression-tests
                     :iclasp iclasp
                     :explicit-inputs (list executable)
                     :explicit-outputs (list (format nil "test-~a" *variant-bitcode-name*)))
  (ninja:write-build output-stream :copy-file
                     :explicit-inputs (list iclasp)
                     :explicit-outputs (list inst-iclasp))
  (loop for input in generated
        for output in products
        do (ninja:write-build output-stream :copy-file
                              :explicit-inputs (list input)
                              :explicit-outputs (list output)))
  (ninja:write-build output-stream :phony
                     :explicit-inputs (list* inst-iclasp
                                             (build-name "install_bitcode")
                                             (image-source configuration :aclasp :install-variant)
                                             (image-source configuration :bclasp :install-variant)
                                             (image-source configuration :cclasp :install-variant)
                                             (build-name "install_modules")
                                             (build-name "install_code" :common t)
                                             (build-name "install_load" :common t)
                                             products)
                     :explicit-outputs (list (build-name "install")))
  (unless (or *variant-prep* *variant-precise*)
    (ninja:write-build output-stream :static-analyzer
                       :iclasp iclasp
                       :variant-path *variant-path*
                       :explicit-inputs (list (make-source (format nil "preciseprep~:[~;-d~]/compile_commands.json"
                                                                       *variant-debug*)
                                                           :build))
                       :implicit-inputs (list executable
                                              (build-name "generated" :prep t :gc :mps))
                       :explicit-outputs (list (build-name "analyze"))
                       :sif

                       (make-source "src/clasp_gc.sif" :code))))

(defmethod print-epilogue (configuration (name (eql :ninja)) output-stream)
  (ninja:write-default output-stream "cclasp-boehm"))

(defmethod print-prologue (configuration (name (eql :config-h)) output-stream)
  (write-line "// Do not edit. Generated by the configure script" output-stream)
  (write-ifndef output-stream "CLASP_CONFIG_H")
  (write-defines output-stream
                 "CLASP_CONFIG_H" t
                 "CST" (cst configuration)
                 "USE_PARALLEL_BUILD" (parallel-build configuration)
                 "CLASP_BUILD_MODE" (position (build-mode configuration)
                                              '(:fasl :object :bitcode :faso :fasoll :fasobc))
                 "USE_COMPILE_FILE_PARALLEL" (if (compile-file-parallel configuration) 1 0)
                 "FORCE_STARTUP_EXTERNAL_LINKAGE" (if (force-startup-external-linkage configuration) 1 0)
                 "USE_PRECISE_GC" *variant-precise*
                 "USE_BOEHM" (eq :boehm *variant-gc*)
                 "USE_MMTK" (eq :mmtk *variant-gc*)
                 "USE_MPS" (eq :mps *variant-gc*)
                 "RUNNING_PRECISEPREP" *variant-prep*
                 "PROGRAM_CLASP" t
                 "CLASP_THREADS" t
                 "CLASP_GIT_COMMIT" (git-commit :short t)
                 "CLASP_GIT_FULL_COMMIT" (git-commit)
                 "CLASP_VERSION" (git-describe)
                 "CLBIND_DYNAMIC_LINK" t
                 "DEFINE_CL_SYMBOLS" t
                 "USE_SOURCE_DATABASE" t
                 "USE_COMPILED_CLOSURE" t
                 "CLASP_UNICODE" t
                 "INCLUDED_FROM_CLASP" t
                 "INHERITED_FROM_SRC" t
                 "NDEBUG" t
                 "X86_64" t
                 "_ADDRESS_MODEL_64" t
                 "__STDC_CONSTANT_MACROS" t
                 "__STDC_FORMAT_MACROS" t
                 "__STDC_LIMIT_MACROS" t
                 "ENABLE_BACKTRACE_ARGS" t
                 "DEBUG_MONITOR_SUPPORT" t
                 "BUILD_LIB" (string-trim " "
                                          (concatenate 'string
                                                       *variant-ldlibs*
                                                       " "
                                                       (ldlibs configuration)))
                 "BUILD_STLIB" ""
                 "BUILD_LINKFLAGS" (string-trim " "
                                                (concatenate 'string
                                                             *variant-ldflags*
                                                             " "
                                                             (ldflags configuration)))
                 "BUILD_CPPFLAGS" (string-trim " "
                                               (concatenate 'string
                                                            *variant-cxxflags*
                                                            " "
                                                            (cxxflags configuration)))
                 "EXECUTABLE_NAME" (build-name :iclasp)
                 "PREFIX" (prefix configuration)
                 "APP_NAME" "CLASP"
                 "BITCODE_NAME" *variant-bitcode-name*
                 "VARIANT_NAME" *variant-name*
                 "VARIANT_DIR" *variant-bitcode-name*
                 "CLASP_CLANG_PATH" (namestring (cc configuration)))
  (loop for os in +os-features+
        when (member os *features*)
          do (write-defines output-stream
                            (format nil "_TARGET_OS_~A" os)
                            t))
  (destructuring-bind (major minor &rest junk)
      (uiop:parse-version (llvm-version configuration))
    (declare (ignore junk))
    (write-defines output-stream
                   "LLVM_VERSION_X100" (+ (* 100 major) minor)
                   "LLVM_VERSION" (+ major (* 0.01 minor))
                   "LLVM_VERSION_INT" major))
  (if *variant-debug*
      (write-defines output-stream
                     "_DEBUG_BUILD" t
                     "DEBUG_DTREE_INTERPRETER" (debug-dtree-interpreter configuration)
                     "DEBUG_DTRACE_LOCK_PROBE" (debug-dtrace-lock-probe configuration)
                     "DEBUG_STACKMAPS" (debug-stackmaps configuration)
                     "DEBUG_ASSERT" (debug-assert configuration)
                     "DEBUG_ASSERT_TYPE_CAST" (debug-assert-type-cast configuration)
                     "SOURCE_DEBUG" (source-debug configuration)
                     "DEBUG_JIT_LOG_SYMBOLS" (debug-jit-log-symbols configuration)
                     "DEBUG_GUARD" (debug-guard configuration)
                     "DEBUG_GUARD_VALIDATE" (debug-guard-validate configuration)
                     "DEBUG_GUARD_BACKTRACE" (debug-guard-backtrace configuration)
                     "DEBUG_GUARD_EXHAUSTIVE_VALIDATE" (debug-guard-exhaustive-validate configuration)
                     "DEBUG_TRACE_INTERPRETED_CLOSURES" (debug-trace-interpreted-closures configuration)
                     "DEBUG_ENVIRONMENTS" (debug-environments configuration)
                     "DEBUG_RELEASE" (debug-release configuration)
                     "DEBUG_CACHE" (debug-cache configuration)
                     "DEBUG_BITUNIT_CONTAINER" (debug-bitunit-container configuration)
                     "DEBUG_LEXICAL_DEPTH" (debug-lexical-depth configuration)
                     "DEBUG_FLOW_TRACKER" (debug-flow-tracker configuration)
                     "DEBUG_DYNAMIC_BINDING_STACK" (debug-dynamic-binding-stack configuration)
                     "DEBUG_VALUES" (debug-values configuration)
                     "DEBUG_IHS" (debug-ihs configuration)
                     "DEBUG_TRACK_UNWINDS" (debug-track-unwinds configuration)
                     "DEBUG_NO_UNWIND" (debug-no-unwind configuration)
                     "DEBUG_STARTUP" (debug-startup configuration)
                     "DEBUG_REHASH_COUNT" (debug-rehash-count configuration)
                     "DEBUG_MONITOR" (debug-monitor configuration)
                     "DEBUG_MONITOR_SUPPORT" (debug-monitor-support configuration)
                     "DEBUG_MEMORY_PROFILE" (debug-memory-profile configuration)
                     "DEBUG_BCLASP_LISP" (debug-bclasp-lisp configuration)
                     "DEBUG_CCLASP_LISP" (debug-cclasp-lisp configuration)
                     "DEBUG_COUNT_ALLOCATIONS" (debug-count-allocations configuration)
                     "DEBUG_COMPILER" (debug-compiler configuration)
                     "DEBUG_VERIFY_MODULES" (debug-verify-modules configuration)
                     "DEBUG_LONG_CALL_HISTORY" (debug-long-call-history configuration)
                     "DEBUG_BOUNDS_ASSERT" (debug-bounds-assert configuration)
                     "DEBUG_GFDISPATCH" (debug-gfdispatch configuration)
                     "DEBUG_FASTGF" (debug-fastgf configuration)
                     "DEBUG_SLOT_ACCESSORS" (debug-slot-accessors configuration)
                     "DEBUG_THREADS" (debug-threads configuration)
                     "DEBUG_STORES" (debug-stores configuration)
                     "DEBUG_ENSURE_VALID_OBJECT" (debug-ensure-valid-object configuration)
                     "DEBUG_QUICK_VALIDATE" (debug-quick-validate configuration)
                     "DEBUG_MPS_SIZE" (debug-mps-size configuration)
                     "DEBUG_MPS_UNDERSCANNING" (debug-mps-underscanning configuration)
                     "DEBUG_DONT_OPTIMIZE_BCLASP" (debug-dont-optimize-bclasp configuration)
                     "DEBUG_RECURSIVE_ALLOCATIONS" (debug-recursive-allocations configuration)
                     "DEBUG_ALLOC_ALIGNMENT" (debug-alloc-alignment configuration)
                     "DEBUG_LLVM_OPTIMIZATION_LEVEL_0" (debug-llvm-optimization-level-0 configuration)
                     "DEBUG_SLOW" (debug-slow configuration)
                     "USE_HUMAN_READABLE_BITCODE" (human-readable-bitcode configuration)
                     "DEBUG_COMPILE_FILE_OUTPUT_INFO" (debug-compile-file-output-info configuration)
                     "CONFIG_VAR_COOL" (config-var-cool configuration))
      (write-defines output-stream
                     "_RELEASE_BUILD" t
                     "ALWAYS_INLINE_MPS_ALLOCATIONS" (always-inline-mps-allocations configuration))))

(defmethod print-epilogue (configuration (name (eql :config-h)) output-stream)
  (write-endif output-stream))

(defmethod print-variant-target-sources
    (configuration (name (eql :config-h)) output-stream
     (target (eql :extension-load)) sources &key &allow-other-keys)
  (write-defines output-stream
                 "BUILD_EXTENSION" (and sources t)
                 "CLASP_EXTENSION_STARTUP_LOADS"
                 (format nil "[~{'~a'~^, ~}]" (mapcar (lambda (source)
                                                        (file-namestring (source-path source)))
                                                      sources))))

(defmethod print-variant-target-sources
    (configuration (name (eql :compile-commands)) output-stream (target (eql :iclasp)) sources
     &key &allow-other-keys)
  (shasht:write-json (loop with build-path = (merge-pathnames (build-path configuration)
                                                              (uiop:getcwd))
                           for source in sources
                           for out = (resolve-source (make-source-output source :type "o"))
                           when (typep source 'cc-source)
                             collect `(:object-plist "directory" ,build-path
                                                     "file" ,(resolve-source source)
                                                     "output" ,out
                                                     "command" ,(format nil "~a ~a ~a -c -MD -MF ~a.d -o~a ~a"
                                                                        (cxx configuration)
                                                                        *variant-cxxflags*
                                                                        (cxxflags configuration)
                                                                        out
                                                                        out
                                                                        (resolve-source source))))
                     output-stream))

