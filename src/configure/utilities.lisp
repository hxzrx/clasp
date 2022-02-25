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
