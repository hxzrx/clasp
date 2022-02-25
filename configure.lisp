(require :asdf)

(defun sync-repo (&key directory repository commit &allow-other-keys)
  (cond ((probe-file directory)
         (format t "Fetching ~A~%" repository)
         (uiop:run-program (list "git" "fetch" "--quiet")
                           :output :interactive
                           :error-output :output
                           :directory directory))
       (t
        (format t "Cloning ~A~%" repository)
        (uiop:run-program (list "git" "clone" repository (namestring directory))
                          :output :interactive
                          :error-output :output)))
  (when commit
    (format t "Checking out ~A from ~A~%" commit repository)
    (uiop:run-program (list "git" "checkout" "--quiet" commit)
                      :output :interactive
                      :error-output :output
                      :directory directory)))

(defun remove-repo (&key directory repository &allow-other-keys)
  (cond ((probe-file directory)
         (format t "Removing clone of ~A at ~A~%" repository directory)
         (uiop:delete-directory-tree (truename directory)
                                     :validate t))
        (t
         (format t "Nothing to be done for ~A~%" repository))))

(let* ((initargs (ignore-errors (uiop:read-file-form #P"config.sexp")))
       (git (getf initargs :git "git"))
       (build (getf initargs :build-path "build/"))
       (extensions (getf initargs :extensions))
       (code (uiop:getcwd)))
  ;; Get all the external dependencies
  (format t "Synchronizing external repositories~%~%")
  #+(or)(loop for source in (uiop:read-file-form #P"repos.sexp")
        for extension = (getf source :extension)
        if (or (not extension)
               (member extension extensions))
          do (apply #'sync-repo source)
        else
          do (apply #'remove-repo source)
        do (terpri))
  ;; Do the absolute minimum to inform ASDF about the location of systems
  ;; in order to find the clasp root and the desired build directory. We
  ;; don't add the entire clasp tree to ASDF since there might be multiple
  ;; build directories already present inside the clasp tree.
  (asdf:initialize-source-registry
    `(:source-registry (:tree ,(uiop:getcwd))
                       :inherit-configuration))
  (asdf:initialize-output-translations
    `(:output-translations (t (,(merge-pathnames (merge-pathnames #P"host-fasl/" build)
                                                 (uiop:getcwd))
                               :implementation))
                           :inherit-configuration))
  (asdf:load-system :configure)
  (apply #'uiop:symbol-call "CONFIGURE" "CONFIGURE" initargs))
  
