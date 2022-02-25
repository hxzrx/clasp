(in-package #:ninja)

(defparameter *indent* nil)
(defparameter *ignored-bindings* nil)
(defparameter *line-end* "")
(defparameter *line-start* "")
(defparameter *line-width* 100)

(defclass wrapping-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((stream :reader wrapping-stream-stream
           :initarg :stream)
   (buffer :reader wrapping-stream-buffer
           :initform (make-array 100 :adjustable t :fill-pointer 0 :element-type 'character))
   (bufferp :accessor wrapping-stream-bufferp
            :initform nil)
   (column :accessor wrapping-stream-column
           :initform 0)))

(defmethod trivial-gray-streams:stream-write-char ((stream wrapping-stream) char)
  (with-accessors ((target wrapping-stream-stream)
                   (buffer wrapping-stream-buffer)
                   (bufferp wrapping-stream-bufferp)
                   (column wrapping-stream-column))
      stream
    (flet ((dump-buffer ()
             (let ((text (concatenate 'string *line-start*
                                      (string-left-trim " "
                                                        (concatenate 'string buffer
                                                                     (string char))))))
               (write-line *line-end* target)
               (write-string text target)
               (setf column (length text)
                     (fill-pointer buffer) 0
                     bufferp nil))))
    (cond ((char= #\Newline char)
           (when bufferp
             (write-string buffer target)
             (setf (fill-pointer buffer) 0
                   bufferp nil))
           (terpri target)
           (setf column 0))
          ((and bufferp
                (>= column (- *line-width* (length *line-end*))))
           (dump-buffer))
          ((and (char/= #\Space char)
                bufferp)
           (vector-push-extend char buffer)
           (incf column))
          ((char/= #\Space char)
           (write-char char target)
           (incf column))
          ((and bufferp
                (= (1- (length buffer))
                   (position #\Space buffer :from-end t)))
           (vector-push-extend char buffer)
           (incf column))
          (t
           (write-string buffer target)
           (setf (fill-pointer buffer) 0
                 bufferp t)
           (vector-push-extend char buffer)
           (incf column))))))

(defmethod trivial-gray-streams:stream-finish-output ((stream wrapping-stream))
  (finish-output (wrapping-stream-stream stream)))

(defmethod trivial-gray-streams:stream-line-column ((stream wrapping-stream))
  (wrapping-stream-column stream))

(defmethod cl:close ((stream wrapping-stream) &key abort)
  (close (wrapping-stream-stream stream)))

(defun write-comment (output-stream text)
  (let ((*line-end* "")
        (*line-start* (format nil "~:[~;  ~]# " *indent*)))
    (format output-stream "~:[~;  ~]# ~a~%"
            *indent* text)))

(defun write-bindings (output-stream &rest bindings)
  (let ((*line-end* " $")
        (*line-start* (format nil "~:[~;  ~]  " *indent*)))
    (loop for (name value) on bindings by #'cddr
          unless (member name *ignored-bindings*)
            do (format output-stream "~:[~;  ~]~(~a~) = ~a~%"
                       *indent* name value))))

(defun write-rule (output-stream name
                   &rest bindings
                   &key command description depfile generator pool restat
                        rspfile rspfile-content deps
                   &allow-other-keys)
  (declare (ignore command description depfile generator pool restat rspfile rspfile-content deps))
  (let ((*line-end* " $")
        (*line-start* "    "))
    (format output-stream "rule ~(~a~)~%" name))
  (let ((*indent* t))
    (apply #'write-bindings output-stream bindings))
  (terpri output-stream))

(defun write-build (output-stream name
                    &rest bindings
                    &key implicit-outputs explicit-outputs implicit-inputs
                         explicit-inputs order-only-inputs
                    &allow-other-keys)
  (let ((*line-end* " $")
        (*line-start* "    "))
    (format output-stream "build~{ ~a~}~@[ |~{ ~a~}~]: ~(~a~)~{ ~a~}~@[ |~{ ~a~}~]~@[ ||~{ ~a~}~]~%"
            explicit-outputs implicit-outputs name
            explicit-inputs implicit-inputs order-only-inputs))
  (let ((*indent* t)
        (*ignored-bindings* '(:implicit-outputs :explicit-outputs :implicit-inputs
                              :explicit-inputs :order-only-inputs)))
    (apply #'write-bindings output-stream bindings))
  (terpri output-stream))

(defun write-default (output-stream &rest targets)
  (let ((*line-end* " $")
        (*line-start* "  "))
    (format output-stream "default~{ ~a~}~%" targets))
  (terpri output-stream))

(defun write-pool (output-stream name
                   &rest bindings
                   &key depth
                   &allow-other-keys)
  (declare (ignore depth))
  (let ((*line-end* " $")
        (*line-start* "    "))
    (format output-stream "pool ~(~a~)~%" name))
  (let ((*indent* t))
    (apply #'write-bindings output-stream bindings))
  (terpri output-stream))

(defun write-include (output-stream path &key new-scope)
  (let ((*line-end* " $")
        (*line-start* "  "))
    (format output-stream "~:[include~;subninja~] ~a~%" new-scope path))
  (terpri output-stream))

