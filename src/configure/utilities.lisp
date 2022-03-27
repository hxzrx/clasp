(in-package #:configure)

(defun message (level control-string &rest args)
  (when (interactive-stream-p *standard-output*)
    (format t "~c[~dm" #\escape
            (case level
              (:err      31)
              (:warn     33)
              (:emph     32)
              (otherwise 0))))
  (apply #'format t control-string args)
  (when (interactive-stream-p *standard-output*)
    (format t "~c[0m" #\escape))
  (terpri *standard-output*)
  (when (eq level :err)
    (uiop:quit 1)))

(defun remove-flag (item flags)
  (let ((pos (search item flags)))
    (cond ((not pos)
           flags)
          ((zerop pos)
           (subseq flags (1+ (length item))))
          (t
           (concatenate 'string (subseq flags 0 (1- pos))
                                (subseq flags (+ pos (length item))))))))

(defun join-plists (plists)
  (loop with result-plist = nil
        for plist in plists
        do (loop for (key value) on plist by #'cddr
                 for current-value = (getf result-plist key)
                 if current-value
                   do (setf (cdr (last current-value))
                            (if (listp value)
                                value
                                (list value)))
                 else
                   do (setf (getf result-plist key)
                            (if (listp value)
                                value
                                (list value))))
        finally (return result-plist)))

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

(defun hidden-component-p (component)
  (equal #\. (uiop:first-char component)))
