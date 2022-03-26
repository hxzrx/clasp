(in-package #:configure)

(defparameter *configuration* nil)
(defparameter *variant-gc* nil)
(defparameter *variant-precise* nil)
(defparameter *variant-prep* nil)
(defparameter *variant-debug* nil)
(defparameter *variant-name* nil)
(defparameter *variant-bitcode-name* nil)
(defparameter *variant-cflags* nil)
(defparameter *variant-cppflags* nil)
(defparameter *variant-cxxflags* nil)
(defparameter *variant-ldflags* nil)
(defparameter *variant-ldlibs* nil)

(defclass flags ()
  ((cflags :accessor cflags
           :initform nil
           :initarg :cflags)
   (cppflags :accessor cppflags
                       :initform nil
                       :initarg :cppflags)
   (cxxflags :accessor cxxflags
             :initform nil
             :initarg :cxxflags)
   (ldflags :accessor ldflags
            :initform nil
            :initarg :ldflags)
   (ldlibs :accessor ldlibs
           :initform nil
           :initarg :ldlibs)))

(defclass variant (flags)
  ((gc :reader variant-gc
       :initarg :gc
       :type (member :boehm :mmtk :mps))
   (precise :reader variant-precise
            :initarg :precise
            :initform nil
            :type boolean)
   (prep :reader variant-prep
         :initarg :prep
         :initform nil
         :type boolean)
   (debug :reader variant-debug
          :initarg :debug
          :initform nil
          :type boolean)))

(defclass configuration (flags)
  ((build-mode :accessor build-mode
               :initarg :build-mode
               :initform :faso
               :type (member :faso :bitcode :object :fasoll :fasobc :fasl)
               :documentation "Define how clasp is built.
- :bitcode compiles to bitcode and thinLTO is used to link everything.
  This gives the fastest product but linking takes a long time.
- :object produces object files and regular linking is used.
  This is probably not as fast as bitcode (maybe a few percent slower)
  but it links fast.
- :faso generates faso files. This is good for development.")
   (build-path :reader build-path
               :initarg :build-path
               :initform #P"build/")
   (parallel-build :accessor parallel-build
                   :initarg :parallel-build
                   :initform t
                   :documentation "Build clasp in parallel")
   (prefix :accessor prefix
           :initform "/usr/"
           :initarg :prefix)
   (package-path :accessor package-path
                 :initform nil
                 :initarg :package-path)
   (extensions :accessor extensions
               :initarg :extensions
               :initform nil)
   (cst :accessor cst
        :initarg :cst
        :initform t)
   (clang-cpp :accessor clang-cpp
              :initarg :clang-cpp
              :initform t
              :documentation "If t use clang-cpp otherwise use the individual clang libraries.")
   (compile-file-parallel :accessor compile-file-parallel
                          :initarg :compile-file-parallel
                          :initform t)
   (force-startup-external-linkage :accessor force-startup-external-linkage
                                   :initarg :force-startup-external-linkage
                                   :initform t
                                   :documentation "Use external-linkage for StartUp functions")
   (unwinder :accessor unwinder
             :initform :gcc
             :initarg :unwinder
             :type (member :gcc :llvm))
   (jobs :accessor jobs
         :initarg :jobs
         :initform nil)
   (always-inline-mps-allocations :accessor always-inline-mps-allocations
                                  :initform t
                                  :initarg :always-inline-mps-allocations)
   (address-sanitizer :accessor address-sanitizer
                      :initarg :address-sanitizer
                      :initform nil
                      :type boolean)
   (memory-sanitizer :accessor memory-sanitizer
                      :initarg :memory-sanitizer
                      :initform nil
                      :type boolean)
   (thread-sanitizer :accessor thread-sanitizer
                      :initarg :thread-sanitizer
                      :initform nil
                      :type boolean)
   (debug-dtree-interpreter :accessor debug-dtree-interpreter
                            :initarg :debug-dtree-interpreter
                            :initform nil
                            :type boolean
                            :documentation "Generate dtree interpreter log")
   (debug-dtrace-lock-probe :accessor debug-dtrace-lock-probe
                            :initarg :debug-dtrace-lock-probe
                            :initform nil
                            :type boolean
                            :documentation "Add a Dtrace probe for mutex lock acquisition")
   (debug-stackmaps :accessor debug-stackmaps
                    :initarg :debug-stackmaps
                    :initform nil
                    :type boolean
                    :documentation "print messages about stackmap registration")
   (debug-assert :accessor debug-assert
                 :initarg :debug-assert
                 :initform t
                 :type boolean
                 :documentation "Turn on DEBUG_ASSERT")
   (debug-assert-type-cast :accessor debug-assert-type-cast
                           :initarg :debug-assert-type-cast
                           :initform nil
                           :type boolean
                           :documentation "Turn on type checking when passing arguments")
   (source-debug :accessor source-debug
                 :initarg :source-debug
                 :initform nil
                 :type boolean
                 :documentation "Allow LOG messages to print - works with CLASP_DEBUG environment variable")
   (debug-jit-log-symbols :accessor debug-jit-log-symbols
                          :initarg :debug-jit-log-symbols
                          :initform nil
                          :type boolean
                          :documentation "Generate a log of JITted symbols in /tmp/clasp-symbols-<pid>")
   (debug-guard :accessor debug-guard
                :initarg :debug-guard
                :initform nil
                :type boolean
                :documentation "Add guards around allocated objects")
   (debug-guard-validate :accessor debug-guard-validate
                         :initarg :debug-guard-validate
                         :initform nil
                         :type boolean
                         :documentation "Add quick checking of guards")
   (debug-guard-backtrace :accessor debug-guard-backtrace
                          :initarg :debug-guard-backtrace
                          :initform nil
                          :type boolean
                          :documentation "Add allocation backtraces to guards")
   (debug-guard-exhaustive-validate :accessor debug-guard-exhaustive-validate
                                    :initarg :debug-guard-exhaustive-validate
                                    :initform nil
                                    :type boolean
                                    :documentation "Add exhaustive, slow, checks of guards")
   (debug-trace-interpreted-closures :accessor debug-trace-interpreted-closures
                                     :initarg :debug-trace-interpreted-closures
                                     :initform nil
                                     :type boolean
                                     :documentation "")
   (debug-environments :accessor debug-environments
                       :initarg :debug-environments
                       :initform nil
                       :type boolean
                       :documentation "")
   (debug-release :accessor debug-release
                  :initarg :debug-release
                  :initform nil
                  :type boolean
                  :documentation "Turn off optimization for a few C++ functions; undef this to optimize everything")
   (debug-cache :accessor debug-cache
                :initarg :debug-cache
                :initform nil
                :type boolean
                :documentation "Debug the dispatch caches - see cache.cc")
   (debug-bitunit-container :accessor debug-bitunit-container
                            :initarg :debug-bitunit-container
                            :initform nil
                            :type boolean
                            :documentation "Prints debug info for bitunit containers")
   (debug-lexical-depth :accessor debug-lexical-depth
                        :initarg :debug-lexical-depth
                        :initform nil
                        :type boolean
                        :documentation "Generate tests for lexical closure depths")
   (debug-flow-tracker :accessor debug-flow-tracker
                       :initarg :debug-flow-tracker
                       :initform nil
                       :type boolean
                       :documentation "Record small backtraces to track flow")
   (debug-dynamic-binding-stack :accessor debug-dynamic-binding-stack
                                :initarg :debug-dynamic-binding-stack
                                :initform nil
                                :type boolean
                                :documentation "dynamic variable binding debugging")
   (debug-values :accessor debug-values
                 :initarg :debug-values
                 :initform nil
                 :type boolean
                 :documentation "turn on printing (values x y z) values when core:*debug-values* is not nil")
   (debug-ihs :accessor debug-ihs
              :initarg :debug-ihs
              :initform nil
              :type boolean
              :documentation "")
   (debug-track-unwinds :accessor debug-track-unwinds
                        :initarg :debug-track-unwinds
                        :initform nil
                        :type boolean
                        :documentation "Count cc_unwind calls and report in TIME")
   (debug-no-unwind :accessor debug-no-unwind
                    :initarg :debug-no-unwind
                    :initform nil
                    :type boolean
                    :documentation "Debug intrinsics that say they don't unwind but actually do")
   (debug-startup :accessor debug-startup
                  :initarg :debug-startup
                  :initform nil
                  :type boolean
                  :documentation "Generate per-thread logs in /tmp/dispatch-history/**  of the slow path of fastgf")
   (debug-rehash-count :accessor debug-rehash-count
                       :initarg :debug-rehash-count
                       :initform nil
                       :type boolean
                       :documentation "Keep track of the number of times each hash table has been rehashed")
   (debug-monitor :accessor debug-monitor
                  :initarg :debug-monitor
                  :initform nil
                  :type boolean
                  :documentation "generate logging messages to a file in /tmp for non-hot code")
   (debug-monitor-support :accessor debug-monitor-support
                          :initarg :debug-monitor-support
                          :initform nil
                          :type boolean
                          :documentation "Must be enabled with other options - do this automatically?")
   (debug-memory-profile :accessor debug-memory-profile
                         :initarg :debug-memory-profile
                         :initform nil
                         :type boolean
                         :documentation "Profile memory allocations total size and counter")
   (debug-bclasp-lisp :accessor debug-bclasp-lisp
                      :initarg :debug-bclasp-lisp
                      :initform nil
                      :type boolean
                      :documentation "Generate debugging frames for all bclasp code - like declaim")
   (debug-cclasp-lisp :accessor debug-cclasp-lisp
                      :initarg :debug-cclasp-lisp
                      :initform t
                      :type boolean
                      :documentation "Generate debugging frames for all cclasp code - like declaim (default on)")
   (debug-count-allocations :accessor debug-count-allocations
                            :initarg :debug-count-allocations
                            :initform nil
                            :type boolean
                            :documentation "count per-thread allocations of instances of classes")
   (debug-compiler :accessor debug-compiler
                   :initarg :debug-compiler
                   :initform nil
                   :type boolean
                   :documentation "Turn on compiler debugging")
   (debug-verify-modules :accessor debug-verify-modules
                         :initarg :debug-verify-modules
                         :initform nil
                         :type boolean
                         :documentation "Verify LLVM modules before using them")
   (debug-long-call-history :accessor debug-long-call-history
                            :initarg :debug-long-call-history
                            :initform nil
                            :type boolean
                            :documentation "The GF call histories used to blow up - this triggers an error if they get too long")
   (debug-bounds-assert :accessor debug-bounds-assert
                        :initarg :debug-bounds-assert
                        :initform t
                        :type boolean
                        :documentation "check bounds")
   (debug-gfdispatch :accessor debug-gfdispatch
                     :initarg :debug-gfdispatch
                     :initform nil
                     :type boolean
                     :documentation "debug call history manipulation")
   (debug-fastgf :accessor debug-fastgf
                 :initarg :debug-fastgf
                 :initform nil
                 :type boolean
                 :documentation "generate slow gf dispatch logging and write out dispatch functions to /tmp/dispatch-history-**")
   (debug-slot-accessors :accessor debug-slot-accessors
                         :initarg :debug-slot-accessors
                         :initform nil
                         :type boolean
                         :documentation "GF accessors have extra debugging added to them")
   (debug-threads :accessor debug-threads
                  :initarg :debug-threads
                  :initform nil
                  :type boolean
                  :documentation "")
   (debug-stores :accessor debug-stores
                 :initarg :debug-stores
                 :initform nil
                 :type boolean
                 :documentation "insert a call to cc_validate_tagged_pointer everytime something is written to memory")
   (debug-ensure-valid-object :accessor debug-ensure-valid-object
                              :initarg :debug-ensure-valid-object
                              :initform nil
                              :type boolean
                              :documentation "Defines ENSURE_VALID_OBJECT(x)->x macro - sprinkle these around to run checks on objects")
   (debug-quick-validate :accessor debug-quick-validate
                         :initarg :debug-quick-validate
                         :initform nil
                         :type boolean
                         :documentation "quick/cheap validate if on and comprehensive validate if not")
   (debug-mps-size :accessor debug-mps-size
                   :initarg :debug-mps-size
                   :initform nil
                   :type boolean
                   :documentation "check that the size of the MPS object will be calculated properly by obj_skip")
   (debug-mps-underscanning :accessor debug-mps-underscanning
                            :initarg :debug-mps-underscanning
                            :initform nil
                            :type boolean
                            :documentation "Very expensive - does a mps_arena_collect/mps_arena_release for each allocation")
   (debug-dont-optimize-bclasp :accessor debug-dont-optimize-bclasp
                               :initarg :debug-dont-optimize-bclasp
                               :initform nil
                               :type boolean
                               :documentation "Optimize bclasp by editing llvm-ir")
   (debug-recursive-allocations :accessor debug-recursive-allocations
                                :initarg :debug-recursive-allocations
                                :initform nil
                                :type boolean
                                :documentation "Catch allocations within allocations - MPS hates these")
   (debug-alloc-alignment :accessor debug-alloc-alignment
                          :initarg :debug-alloc-alignment
                          :initform nil
                          :type boolean
                          :documentation "catch misaligned allocations")
   (debug-llvm-optimization-level-0 :accessor debug-llvm-optimization-level-0
                                    :initarg :debug-llvm-optimization-level-0
                                    :initform nil
                                    :type boolean
                                    :documentation "")
   (debug-slow :accessor debug-slow
               :initarg :debug-slow
               :initform nil
               :type boolean
               :documentation "Code runs slower due to checks - undefine to remove checks")
   (human-readable-bitcode :accessor human-readable-bitcode
                           :initarg :human-readable-bitcode
                           :initform nil
                           :type boolean
                           :documentation "")
   (debug-compile-file-output-info :accessor debug-compile-file-output-info
                                   :initarg :debug-compile-file-output-info
                                   :initform nil
                                   :type boolean
                                   :documentation "")
   (config-var-cool :accessor config-var-cool
                    :initarg :config-var-cool
                    :initform t
                    :type boolean
                    :documentation "mps setting")
   (ld :accessor ld
       :initarg :ld
       :initform #+darwin nil #-darwin :gold
       :type (member :bfd :lld :gold :mold)
       :documentation "The linker to use")
   (ar :accessor ar
       :initarg :ar
       :initform nil)
   (cc :accessor cc
       :initarg :cc
       :initform nil)
   (cxx :accessor cxx
        :initarg :cxx
        :initform nil)
   (git :accessor git
        :initarg :git
        :initform nil)
   (llvm-config :accessor llvm-config
                :initform nil
                :initarg :llvm-config)
   (nm :accessor nm
       :initarg :nm
       :initform nil)
   (pkg-config :accessor pkg-config
               :initform nil
               :initarg :pkg-config)
   (scripts :accessor scripts
            :initform '(#P"cscript.lisp"))
   (units :accessor units
          :initform '(:cpu-count :base :pkg-config :clang :llvm))
   (outputs :accessor outputs
            :initform (alexandria:plist-hash-table (list :generate-sif
                                                         (list (make-source #P"generate-sif.lisp" :build))
                                                         :generate-headers
                                                         (list (make-source #P"generate-headers.lisp" :build))
                                                         :compile-aclasp
                                                         (list (make-source #P"compile-aclasp.lisp" :build))
                                                         :compile-bclasp
                                                         (list (make-source #P"compile-bclasp.lisp" :build))
                                                         :compile-cclasp
                                                         (list (make-source #P"compile-cclasp.lisp" :build))
                                                         :compile-module
                                                         (list (make-source #P"compile-module.lisp" :build))
                                                         :link-fasl
                                                         (list (make-source #P"link-fasl.lisp" :build))
                                                         :static-analyzer
                                                         (list (make-source #P"static-analyzer.lisp" :variant))
                                                         :ninja
                                                         (list (make-source #P"build.ninja" :build)
                                                               :bitcode :iclasp :aclasp :bclasp :cclasp
                                                               :modules :extension-load :install-code :clasp)
                                                         :config-h
                                                         (list (make-source #P"config.h" :variant)
                                                               :extension-load)
                                                         :compile-commands
                                                         (list (make-source #P"compile_commands.json" :variant)
                                                               :iclasp))))
   (targets :accessor targets
            :initform (make-hash-table))
   (llvm-version :accessor llvm-version
                 :initform nil)
   (llvm-bindir :accessor llvm-bindir
                :initform nil)
   (llvm-includedir :accessor llvm-includedir
                    :initform nil)
   (scraper-headers :accessor scraper-headers
                     :initform nil)
   (scraper-precise-headers :accessor scraper-precise-headers
                             :initform nil)
   (scraper-lisp-sources :accessor scraper-lisp-sources
                     :initform nil)
   (variants :reader variants
             :initform (list (make-instance 'variant :gc :boehm)
                             (make-instance 'variant :gc :boehm :precise t)
                             (make-instance 'variant :gc :boehm :debug t)
                             (make-instance 'variant :gc :boehm :precise t :debug t)
                             (make-instance 'variant :gc :mps :prep t)
                             (make-instance 'variant :gc :mps :debug t :prep t)))))

(defun build-name (name
                   &key common
                        (gc *variant-gc* gc-p)
                        (precise *variant-precise* precise-p)
                        (prep *variant-prep* prep-p)
                        (debug *variant-debug* debug-p))
  (format nil "~(~a~)-~a" name
          (cond (common
                 "common")
                ((or gc-p precise-p prep-p debug-p)
                 (variant-bitcode-name (make-instance 'variant :gc gc :precise precise
                                                      :prep prep :debug debug)))
                (t
                 *variant-bitcode-name*))))

(defun file-faso-extension (configuration)
  (case (build-mode configuration)
    (:faso "faso")
    (:fasobc "fasobc")
    (:fasoll "fasoll")
    (otherwise "fasl")))

(defun module-fasl-extension (configuration)
  (case (build-mode configuration)
    (:faso "fasp")
    (:fasobc "faspbc")
    (:fasoll "faspll")
    (otherwise "fasl")))

(defun image-fasl-extension (configuration)
  (case (build-mode configuration)
    (:fasl "lfasl")
    (:faso "fasp")
    (:fasobc "faspbc")
    (:fasoll "faspll")
    (otherwise "fasl")))

(defun image-source (configuration target &optional (root :variant))
  (make-source (format nil "fasl/~(~a~)-~a-image.~a"
                       target *variant-bitcode-name*
                       (image-fasl-extension configuration))
               root))

(defun funcall-variant (configuration func
                        &key (debug nil debug-p) (gc nil gc-p)
                             (precise nil precise-p) (prep nil prep-p)
                        &allow-other-keys)
  (if (or debug-p gc-p precise-p prep-p)
      (loop for variant in (variants configuration)
            when (and (or (not debug-p)
                          (eql debug (variant-debug variant)))
                      (or (not gc-p)
                          (eql gc (variant-gc variant)))
                      (or (not precise-p)
                          (eql precise (variant-precise variant)))
                      (or (not prep-p)
                          (eql prep (variant-prep variant))))
              do (funcall func variant))
      (funcall func configuration)))

(defun append-cflags (configuration flags &rest rest
                      &key type &allow-other-keys
                      &aux (trimmed-flags (string-trim " " flags)))
  (unless (zerop (length trimmed-flags))
    (apply #'funcall-variant
           configuration
           (lambda (object)
             (when (or (null type)
                       (eql :cflags type))
               (setf (cflags object)
                     (format nil "~@[~a ~]~a" (cflags object) trimmed-flags)))
             (when (or (null type)
                       (eql :cxxflags type))
               (setf (cxxflags object)
                     (format nil "~@[~a ~]~a" (cxxflags object) trimmed-flags)))
             (when (or (null type)
                       (eql :cppflags type))
               (setf (cppflags object)
                     (format nil "~@[~a ~]~a" (cppflags object) trimmed-flags))))
          rest)))

(defun append-ldflags (configuration flags &rest rest
                       &aux (trimmed-flags (string-trim " " flags)))
  (unless (zerop (length trimmed-flags))
    (apply #'funcall-variant
           configuration
           (lambda (object)
             (setf (ldflags object)
                   (format nil "~@[~a ~]~a" (ldflags object) trimmed-flags)))
           rest)))

(defun append-ldlibs (configuration flags &rest rest
                       &aux (trimmed-flags (string-trim " " flags)))
  (unless (zerop (length trimmed-flags))
    (apply #'funcall-variant
           configuration
           (lambda (object)
             (setf (ldlibs object)
                   (format nil "~@[~a ~]~a" (ldlibs object) trimmed-flags)))
           rest)))

(defun sources (target &rest sources)
  (loop for source in sources
        do (add-target-source *configuration* target source)))

(defun recurse (&rest paths)
  (loop with script-path = *script-path*
        for path in paths
        do (message :info "Looking for configure scripts in ~a" path)
           (loop for subpath in (directory (merge-pathnames (merge-pathnames "cscript.lisp" path) script-path))
                 for script = (uiop:subpathp subpath (uiop:getcwd))
                 for *script-path* = (uiop:pathname-directory-pathname script)
                 do (message :info "Loading script ~a" script)
                    (load script))))

(defun run-program (command &key directory)
  (uiop:run-program command
                    :directory directory
                    :output :interactive
                    :error-output :output))

(defun run-program-capture (command &key directory)
  (ignore-errors
    (multiple-value-bind (standard-output error-output code)
        (uiop:run-program command
                          :directory directory
                          :ignore-error-status t
                          :output '(:string :stripped t)
                          :error-output '(:string :stripped t))
      (declare (ignore error-output))
      (when (zerop code)
        standard-output))))
                                          
(defun git-clone (repository directory &optional branch)
  (unless (probe-file (resolve-source directory))
    (message :info "Cloning ~A" repository)
    (run-program (list "git" "clone" repository directory)))
  (when branch
    (message :info "Checking out ~A from ~A" branch repository)
    (run-program (list "git" "checkout" "--quiet" branch) :directory (resolve-source directory))))

(defun git-commit (&key short)
  (run-program-capture (format nil "git rev-parse~:[~; --short~] HEAD" short)))

(defun git-describe ()
  (run-program-capture (format nil "git describe --always")))

(defun add-output (name path &rest targets)
  (setf (gethash name (outputs *configuration*))
        (cons path targets)))

(defgeneric configure-unit (configuration unit))

(defgeneric make-output-stream (configuration name path)
  (:method (configuration name path)
    (declare (ignore configuration name))
    (open path :direction :output :if-exists :supersede
          :if-does-not-exist :create)))

(defgeneric add-target-source (configuration target source)
  (:method (configuration target source)
    (unless (find source (gethash target (targets *configuration*))
                  :test (lambda (x y)
                          (and (equal (source-path x) (source-path y))
                               (eql (source-root x) (source-root y)))))
      (setf (gethash target (targets *configuration*))
            (nconc (gethash target (targets *configuration*))
                   (list source))))))

(defun hidden-component (component)
  (equal #\. (uiop:first-char component)))

(defgeneric print-prologue (configuration name output-stream)
  (:method (configuration name output-stream)
    (declare (ignore configuration name output-stream))))

(defgeneric print-epilogue (configuration name output-stream)
  (:method (configuration name output-stream)
    (declare (ignore configuration name output-stream))))

(defgeneric print-target-source (configuration name output-stream target source)
  (:method (configuration name output-stream target source)
    (declare (ignore configuration name output-stream target source))))

(defgeneric print-target-sources (configuration name output-stream target sources &key &allow-other-keys)
  (:method (configuration name output-stream target sources &key &allow-other-keys)
    (declare (ignore configuration name output-stream target sources))))

(defgeneric print-prologue (configuration name output-stream)
  (:method (configuration name output-stream)
    (declare (ignore configuration name output-stream targets))))

(defgeneric print-epilogue (configuration name output-stream)
  (:method (configuration name output-stream)
    (declare (ignore configuration name output-stream targets))))

(defgeneric print-variant-target-source (configuration name output-stream target source)
  (:method (configuration name output-stream target source)
    (declare (ignore configuration name output-stream target source))))

(defgeneric print-variant-target-sources
    (configuration name output-stream target sources &key &allow-other-keys)
  (:method (configuration name output-stream target sources &key &allow-other-keys)
    (declare (ignore configuration name output-stream target sources))))

(defun variant-name (variant)
  (format nil "~:[~(~a~)~:[~;precise~]~;preciseprep~]"
          (variant-prep variant)
          (variant-gc variant)
          (variant-precise variant)))

(defun variant-bitcode-name (variant)
  (format nil "~a~:[~;-d~]" (variant-name variant) (variant-debug variant)))

(defun map-variants (configuration func)
  (loop for variant in (variants configuration)
        for *variant-gc* = (variant-gc variant)
        for *variant-precise* = (variant-precise variant)
        for *variant-prep* = (variant-prep variant)
        for *variant-debug* = (variant-debug variant)
        for *variant-cflags* = (or (cflags variant) "")
        for *variant-cxxflags* = (or (cxxflags variant) "")
        for *variant-cppflags* = (or (cppflags variant) "")
        for *variant-ldflags* = (or (ldflags variant) "")
        for *variant-ldlibs* = (or (ldlibs variant) "")
        for *variant-name* = (variant-name variant)
        for *variant-bitcode-name* = (variant-bitcode-name variant)
        for *variant-path* = (merge-pathnames (make-pathname :directory (list :relative *variant-bitcode-name*))
                                              *build-path*)
        for *install-variant-path* = (merge-pathnames (make-pathname :directory (list :relative "build" *variant-bitcode-name*))
                                                      *install-clasp-path*)
        do (funcall func)))

(defun write-configure-build-output (configuration name)
  (destructuring-bind (path &rest targets)
      (gethash name (outputs configuration))
    (when (eq (source-root path) :build)
      (message :info "Writing output ~a" (resolve-source path))
      (ensure-directories-exist (resolve-source path))
      (let* ((stream (make-output-stream configuration name (resolve-source path)))
             (*build-path* #P"")
             (*code-path* (make-pathname :directory '(:relative :up))))
        (unwind-protect
            (progn
              (print-prologue configuration name stream)
              (loop for target in targets
                    for sources = (gethash target (targets configuration))
                    do (apply #'print-target-sources configuration name
                              stream target sources
                              (join-plists (loop for source in sources
                                                 collect (print-target-source configuration name
                                                                              stream target source)))))
              (map-variants configuration
                            (lambda ()
                              (loop for target in targets
                                    for sources = (gethash target (targets configuration))
                                    do (apply #'print-variant-target-sources configuration name
                                              stream target sources
                                              (join-plists (loop for source in sources
                                                                 collect (print-variant-target-source configuration name
                                                                                                      stream target source)))))))
              (print-epilogue configuration name stream))
          (close stream))))))

(defun write-configure-variant-output (configuration name)
  (destructuring-bind (path &rest targets)
      (gethash name (outputs configuration))
    (when (eq (source-root path) :variant)
      (message :info "Writing output ~a" (resolve-source path))
      (ensure-directories-exist (resolve-source path))
      (let* ((stream (make-output-stream configuration name (resolve-source path)))
             (*build-path* #P"")
             (*variant-path* (make-pathname :directory (list :relative *variant-bitcode-name*)))
             (*install-variant-path* (merge-pathnames (make-pathname :directory (list :relative
                                                                                      "build"
                                                                                      *variant-bitcode-name*))
                                                      *install-clasp-path*))
             (*code-path* (make-pathname :directory '(:relative :up))))
        (unwind-protect
            (progn
              (print-prologue configuration name stream)
              (loop for target in targets
                    for sources = (gethash target (targets configuration))
                    do (apply #'print-variant-target-sources configuration name
                              stream target sources
                              (join-plists (loop for source in sources
                                                     collect (print-variant-target-source configuration name
                                                                                          stream target source)))))
              (print-epilogue configuration name stream))
          (close stream))))))

(defun configure (&rest initargs)
  (let* ((*configuration* (apply #'make-instance 'configuration initargs))
         (*code-path* #P"")
         (*build-path* (build-path *configuration*))
         (*script-path* #P"")
         (prefix (if (package-path *configuration*)
                     (merge-pathnames (uiop:relativize-pathname-directory (prefix *configuration*))
                                      (package-path *configuration*))
                     (prefix *configuration*)))
         (*install-bin-path* (merge-pathnames #P"bin/" prefix))
         (*install-clasp-path* (merge-pathnames #P"lib/clasp/" prefix)))
    (ensure-directories-exist (build-path *configuration*))
    (loop for unit in (units *configuration*)
          do (configure-unit *configuration* unit))
    (recurse #P"")
    (loop for name being the hash-keys in (outputs *configuration*)
          do (write-configure-build-output *configuration* name))
    (map-variants *configuration*
                  (lambda ()
                    (loop for name being the hash-keys in (outputs *configuration*)
                          do (write-configure-variant-output *configuration* name))))))

(defun configure-library (configuration library &rest rest &key required min-version max-version &allow-other-keys)
  (message :info "Configuring library ~a" library)
  (flet ((failure (control-string &rest args)
           (apply #'message (if required :err :warn) control-string args)
           nil))
    (let ((version (run-program-capture (list (pkg-config configuration) "--modversion" library))))
      (cond ((not version)
             (failure "Module ~a not found." library))
            ((and min-version
                  (uiop:version< version min-version))
             (failure "Module ~a with a version of ~a is less then minimum version of ~a." library version min-version))
            ((and max-version
                  (uiop:version<= max-version version))
             (failure "Module ~a with a version of ~a is not less then maximum version of ~a." library version max-version))
            (t
             (apply #'append-cflags configuration
                                    (run-program-capture (list (pkg-config configuration) "--cflags" library))
                                    rest)
             (apply #'append-ldflags configuration
                                     (run-program-capture (list (pkg-config configuration) "--libs-only-L" library))
                                     rest)
             (apply #'append-ldlibs configuration
                                    (run-program-capture (list (pkg-config configuration) "--libs-only-l" library))
                                    rest)
             t)))))

(defun library (&rest rest)
  (apply #'configure-library *configuration* rest))

(defun includes (&rest paths)
  (let ((*build-path* #P"")
        (*code-path* (make-pathname :directory '(:relative :up))))
    (append-cflags *configuration*
                   (format nil "~{-I~a~^ ~}" (mapcar #'resolve-source paths)))))
