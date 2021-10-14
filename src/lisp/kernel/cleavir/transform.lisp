(in-package #:clasp-cleavir)

;;;; TRANSFORMS are like compiler macros, but use the context (environment)
;;;; more heavily. Currently they are implemented through compiler macros,
;;;; but the intent is that in the future they will be used after the source
;;;; stage, when much more information has been made available through analysis.
;;;; Syntax is as follows:
;;;; deftransform (op-name (&rest lambda-list) &body body)
;;;; op-name is the name of a function or macro.
;;;; lambda-list is a typed lambda list, kind of like defmethod, but with
;;;;  types allowed as "specializers". Currently the lambda list can only have
;;;;  required parameters.
;;;; Semantics are as follows:
;;;; When the compiler sees a call to op-name, it will determine the types
;;;; of the argument forms as best it can. Then it will try to find a
;;;; transform such that the argument types are subtypes of the types of the
;;;; transform's lambda list. If it finds one, it calls the transform function
;;;; with the given argument forms. If the transform returns NIL, the compiler
;;;; tries another valid transform if there is one, or else gives up.
;;;; Otherwise, the compiler substitutes the result for the original op-name.
;;;; Here's a simple example:
;;;; (deftransform eql ((x symbol) y) 'eq)
;;;; Now when the compiler sees (eql 'foo x), this transform might be used
;;;; because it's easy to see 'FOO is a symbol. The transform unconditionally
;;;; returns EQ, so the compiler replaces the form with (eq 'foo x) and
;;;; compiles that instead.
;;;; More complicated examples return a lambda expression.
;;;: NOTE: The order in which transforms are tried is not defined.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun values-type-primary (values-type)
    ;; VALUES-TYPE is an actual values type spec, i.e. a cons (values ...)
    ;; get the first thing in the values type - either a scalar type
    ;; or a lambda-list keyword. We assume validity.
    (let ((first (second values-type)))
      (case first
        ((&optional)
         (let ((second (third values-type)))
           (if (eq second '&rest)
               (fourth values-type)
               second)))
        ((&rest) (third values-type))
        (t first))))
  
  (defun function-type-result (type &optional env)
    ;; type is any type specifier
    (multiple-value-bind (head args) (core::normalize-type type env)
      (if (eq head 'function)
          (let ((result (second args)))
            (cond ((null result) 't)
                  ((and (consp result) (eq (car result) 'values))
                   (values-type-primary result))
                  (t result)))
          't)))

  ;;; Given a type, find what type (aref (the TYPE ...) ...) is.
  (defun array-type-element-type (type env)
    (let ((types
            (loop for et in core::+upgraded-array-element-types+
                  ;; Exclude any element type that is CERTAINLY not included.
                  unless (subtypep `(and (array ,et) ,type) nil env)
                    collect et)))
      ;; Now simplify a bit for common stupid cases
      (if (member t types)
          't
          (cons 'or (remove nil types)))))

  ;;; Because some transforms relying on type information are unsafe,
  ;;; we ignore type declarations unless the TRUST-TYPE-DECLARATIONS policy
  ;;; is in place (at low SAFETY). See policy.lisp.
  (defun form-type (form env)
    (let ((trust-type-decls-p
            (environment-has-policy-p env 'ext:assume-right-type)))
      (cond ((constantp form env)
             `(eql ,(ext:constant-form-value form env)))
            ((consp form)
             (let ((operator (first form)))
               (if (symbolp operator)
                   (case operator
                     ((the)
                      (if trust-type-decls-p
                          (let ((type (second form)) (form (third form)))
                            `(and ,(if (and (consp type) (eq (first type) 'values))
                                       (values-type-primary type)
                                       type)
                                  ,(form-type form env)))
                          't))
                     ((function)
                      (if (and (symbolp (second form)) trust-type-decls-p)
                          (let ((info (cleavir-env:function-info
                                       clasp-cleavir:*clasp-system*
                                       env (second form))))
                            (if (typep info '(or cleavir-env:local-function-info
                                              cleavir-env:global-function-info))
                                (cleavir-env:type info)
                                'function))
                          'function))
                     ;; This could be expanded.
                     ((lambda) 'function)
                     ;; This is really KLUDGEy, but then this whole thing kind of is.
                     ((aref)
                      (if (and (consp form) (consp (cdr form)))
                          (let* ((array-form (second form))
                                 (array-form-type (form-type array-form env)))
                            (array-type-element-type array-form-type env))
                          ;; malformed
                          't))
                     (otherwise
                      (if trust-type-decls-p
                          (let ((info (cleavir-env:function-info
                                       clasp-cleavir:*clasp-system*
                                       env operator)))
                            (if (typep info '(or cleavir-env:local-function-info
                                              cleavir-env:global-function-info))
                                (function-type-result (cleavir-env:type info) env)
                                't))
                          't)))
                   't)))
            (t ; symbol (everything else covered by constantp and consp)
             (if trust-type-decls-p
                 (let ((info (cleavir-env:variable-info
                              clasp-cleavir:*clasp-system* env form)))
                   (if info
                       (cleavir-env:type info)
                       't))
                 't)))))

  (defvar *transformers* (make-hash-table :test #'equal))

  (defun maybe-transform (form table args env)
    (let ((argtypes (mapcar (lambda (f) (form-type f env)) args)))
      (with-hash-table-iterator (next table)
        (loop
          (multiple-value-bind (present types transformer) (next)
            (if present
                (when (every (lambda (at ty) (subtypep at ty env)) argtypes types)
                  (let ((res (apply transformer args)))
                    (when res (return `(,res ,@args)))))
                (return form))))))))

;;; Set up an operator as having transforms
(defmacro deftransformation (name)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',name *transformers*)
             (make-hash-table :test #'equalp)))
     (define-cleavir-compiler-macro ,name (&whole form &rest args &environment env)
       (maybe-transform form (gethash ',name *transformers*) args env))))

;;; Main interface
(defmacro deftransform (name (&rest lambda-list) &body body)
  (let ((params (loop for var in lambda-list
                      collect (if (consp var) (car var) var)))
        (types (loop for var in lambda-list
                     collect (if (consp var) (second var) 't))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ;; If the operator hasn't been set up for transformation yet,
       ;; do so implicitly
       (unless (nth-value 1 (gethash ',name *transformers*))
         (deftransformation ,name))
       ;; Actually define the transform
       (setf (gethash ',types (gethash ',name *transformers*))
             (lambda (,@params) (declare (ignorable ,@params)) ,@body)))))

;;;

(deftransform eql ((x (not core::eq-incomparable)) y) 'eq)
(deftransform eql (x (y (not core::eq-incomparable))) 'eq)

(deftransform car ((x cons)) 'cleavir-primop:car)
(deftransform cdr ((x cons)) 'cleavir-primop:cdr)

(deftransform rplaca ((cons cons) value)
  '(lambda (cons value)
    (cleavir-primop:rplaca cons value)
    cons))
(deftransform rplacd ((cons cons) value)
  '(lambda (cons value)
    (cleavir-primop:rplacd cons value)
    cons))

(deftransform primop:inlined-two-arg-+ ((x fixnum) (y fixnum))
  'core:two-arg-+-fixnum-fixnum)

#+(or)
(macrolet ((def-float-op (name op)
             `(progn
                (deftransform ,name ((x single-float) (y single-float))
                  '(lambda (x y) (,op single-float x y)))
                (deftransform ,name ((x single-float) (y double-float))
                  '(lambda (x y)
                    (,op double-float
                     (cleavir-primop:coerce single-float double-float x) y)))
                (deftransform ,name ((x double-float) (y single-float))
                  '(lambda (x y)
                    (,op double-float
                     x (cleavir-primop:coerce single-float double-float y))))
                (deftransform ,name ((x double-float) (y double-float))
                  '(lambda (x y) (,op double-float x y)))))
           (def-float-compare (name op)
             `(progn
                (deftransform ,name ((x single-float) (y single-float))
                  '(lambda (x y) (if (,op single-float x y) t nil)))
                (deftransform ,name ((x double-float) (y double-float))
                  '(lambda (x y) (if (,op double-float x y) t nil))))))
  (def-float-op primop:inlined-two-arg-+ cleavir-primop:float-add)
  (def-float-op primop:inlined-two-arg-- cleavir-primop:float-sub)
  (def-float-op primop:inlined-two-arg-* cleavir-primop:float-mul)
  (def-float-op primop:inlined-two-arg-/ cleavir-primop:float-div)
  (def-float-compare primop:inlined-two-arg-<  cleavir-primop:float-less)
  (def-float-compare primop:inlined-two-arg-<= cleavir-primop:float-not-greater)
  (def-float-compare primop:inlined-two-arg-=  cleavir-primop:float-equal)
  (def-float-compare primop:inlined-two-arg->  cleavir-primop:float-greater)
  (def-float-compare primop:inlined-two-arg->= cleavir-primop:float-not-less))

#+(or)
(macrolet ((def-fixnum-compare (name op)
             `(deftransform ,name ((x fixnum) (y fixnum))
                '(lambda (x y) (if (,op x y) t nil)))))
  (def-fixnum-compare primop:inlined-two-arg-<  cleavir-primop:fixnum-less)
  (def-fixnum-compare primop:inlined-two-arg-<= cleavir-primop:fixnum-not-greater)
  (def-fixnum-compare primop:inlined-two-arg-=  cleavir-primop:fixnum-equal)
  (def-fixnum-compare primop:inlined-two-arg->  cleavir-primop:fixnum-greater)
  (def-fixnum-compare primop:inlined-two-arg->= cleavir-primop:fixnum-not-less))

#+(or)
(deftransform minusp ((number fixnum))
  '(lambda (n) (if (cleavir-primop:fixnum-less n 0) t nil)))
#+(or)
(deftransform plusp ((number fixnum))
  '(lambda (n) (if (cleavir-primop:fixnum-greater n 0) t nil)))

(deftransform array-total-size ((a (simple-array * (*)))) 'core::vector-length)
;;(deftransform array-total-size ((a core:mdarray)) 'core::%array-total-size)

(deftransform array-rank ((a (simple-array * (*)))) '(lambda (a) (declare (ignore a)) 1))

#+(or)
(deftransform svref/no-bounds-check ((a simple-vector) (index fixnum))
  '(lambda (vector index) (cleavir-primop:aref vector index t t t)))
#+(or)
(deftransform (setf svref/no-bounds-check) (value (a simple-vector) (index fixnum))
  '(lambda (value vector index)
    (cleavir-primop:aset vector index value t t t)
    value))

(deftransform length ((s list)) '(lambda (x) (if x (core:cons-length x) 0)))
(deftransform length ((s vector)) 'core::vector-length)

#+(or)
(deftransform elt ((s vector) index) 'vector-read)
#+(or)
(deftransform core:setf-elt (value (s vector) index)
  '(lambda (value sequence index) (vector-set sequence index new-value)))

(deftransform core:coerce-fdesignator ((fd symbol)) 'fdefinition)
(deftransform core:coerce-fdesignator ((fd function)) 'identity)

(defmacro define-bir-transform (fname (instparam) &body body)
  `(setf (gethash ',fname *fn-transforms*)
         (list (lambda (,instparam) ,@body))))

(defun replace-call-with-primop (call primop-name)
  (change-class call 'cleavir-bir:vprimop
                :inputs (rest (bir:inputs call)) ; don't need the function
                :info (cleavir-primop-info:info primop-name)))

;;; This is important to perform complex type derivations.
;;; I have serious reservations about doing it in this side effectual way,
;;; but the perfect is the enemy of the good.
(defun wrap-in-thei (inst ctype)
  (let* ((datum (bir:output inst))
         (new-datum (make-instance 'bir:output
                      :derived-type (bir:ctype datum)))
         (thei (make-instance 'bir:thei
                 :policy (bir:policy inst) :origin (bir:origin inst)
                 :asserted-type ctype :type-check-function :trusted
                 :outputs (list new-datum))))
    (bir:insert-instruction-after thei inst)
    (bir:replace-uses new-datum datum)
    (setf (bir:inputs thei) (list datum))
    thei))

(defun replace-with-primop-and-wrap (inst primop ctype)
  (wrap-in-thei inst (cleavir-ctype:single-value ctype *clasp-system*))
  (replace-call-with-primop inst primop)
  t)

(defun wrap-coerce-sf-to-df (inst datum)
  (let* ((df (cleavir-ctype:single-value
              (cleavir-ctype:range 'double-float '* '* *clasp-system*)
              *clasp-system*))
         (new (make-instance 'bir:output
                :derived-type df))
         (coerce (make-instance 'bir:vprimop
                   :origin (bir:origin inst) :policy (bir:policy inst)
                   :info (cleavir-primop-info:info 'core::single-to-double)
                   :outputs (list new))))
    (bir:insert-instruction-before coerce inst)
    (bir:replace-uses new datum)
    (setf (bir:inputs coerce) (list datum)))
  (values))

(defun arg-subtypep (arg ctype)
  (cleavir-ctype:subtypep (cleavir-ctype:primary (bir:ctype arg)
                                                 *clasp-system*)
                          ctype *clasp-system*))

;; warning: multiply evaluates args
(defmacro subtypepcase (args &rest clauses)
  `(cond ,@(loop for (types . body) in clauses
                 collect `((and ,@(loop for arg in args
                                        for type in types
                                        collect `(arg-subtypep ,arg ,type)))
                           ,@body))))

(macrolet ((define-two-arg-sf (name sf-primop df-primop)
             `(define-bir-transform ,name (call)
                (let ((arguments (rest (bir:inputs call)))
                      (sf (cleavir-ctype:range
                           'single-float '* '* *clasp-system*))
                      (df (cleavir-ctype:range
                           'double-float '* '* *clasp-system*)))
                  (subtypepcase
                   ((first arguments) (second arguments))
                   ((sf sf) (replace-with-primop-and-wrap call ',sf-primop sf))
                   ((df df) (replace-with-primop-and-wrap call ',df-primop df))
                   ((sf df)
                    (wrap-coerce-sf-to-df call (first arguments))
                    (replace-with-primop-and-wrap call ',df-primop df))
                   ((df sf)
                    (wrap-coerce-sf-to-df call (second arguments))
                    (replace-with-primop-and-wrap call ',df-primop df)))))))
  (define-two-arg-sf core:two-arg-+ core::two-arg-sf-+ core::two-arg-df-+)
  (define-two-arg-sf core:two-arg-- core::two-arg-sf-- core::two-arg-df--)
  (define-two-arg-sf core:two-arg-* core::two-arg-sf-* core::two-arg-df-*)
  (define-two-arg-sf core:two-arg-/ core::two-arg-sf-/ core::two-arg-df-/)
  (define-two-arg-sf expt core::sf-expt core::df-expt))

(macrolet ((define-one-arg-sf (name sf-primop df-primop)
             `(define-bir-transform ,name (call)
                (let ((arguments (rest (bir:inputs call)))
                      (sf (cleavir-ctype:range
                           'single-float '* '* *clasp-system*))
                      (df (cleavir-ctype:range
                           'double-float '* '* *clasp-system*)))
                  (subtypepcase
                   ((first arguments))
                   ((sf) (replace-with-primop-and-wrap call ',sf-primop sf))
                   ((df)
                    (replace-with-primop-and-wrap call ',df-primop df)))))))
  (define-one-arg-sf cos core::sf-cos core::df-cos)
  (define-one-arg-sf sin core::sf-sin core::df-sin)
  (define-one-arg-sf abs core::sf-abs core::df-abs)
  (define-one-arg-sf sqrt core::sf-sqrt core::df-sqrt)
  (define-one-arg-sf exp core::sf-exp core::df-exp)
  ;; there's probably some weird floating point reason to explain why
  ;; llvm has an fneg instruction but not a reciprocal, but i don't know it.
  (define-one-arg-sf core:negate core::sf-negate core::df-negate))
