(in-package #:configure)

(defun write-define (output-stream name &optional value)
  (format output-stream "#define ~a~@[ ~a~]~%" name value))

(defun write-ifndef (output-stream name)
  (format output-stream "#ifndef ~a~%" name))

(defun write-endif (output-stream)
  (write-line "#endif" output-stream))

(defun write-defines (output-stream &rest rest)
  (loop for (name value) on rest by #'cddr
        when value
          do (format output-stream "#define ~a~@[ ~a~]~%"
                     name (unless (eq t value)
                            (write-to-string value)))))

(defun write-includes (output-stream &rest rest)
  (format output-stream "~{#include <~a>~%~}" rest))
                            
