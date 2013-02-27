;;;; defrec.lisp
;;;; Copyright (c) 2013 Robert Smith

(in-package #:defrec)

(defmacro defrec (&body definitions)
  "Define mutually recursive functions. Each definition of DEFINITIONS
  should be of the same form of FLET or LABELS:

  (<function name> <lambda list>
    <function body>)."
  (let* ((f-names (mapcar #'first definitions))
         (gensyms (mapcar (lambda (f-name)
                            (gensym (symbol-name f-name)))
                          f-names))
         (g-defns (mapcar (lambda (g defn)
                            (cons g (cdr defn)))
                          gensyms
                          definitions)))
    `(labels (,@definitions)
       (flet ,g-defns
         (declare (inline ,@gensyms))
         ,@(mapcar (lambda (f-name g-defn)
                     `(cl:defun ,f-name ,(second g-defn)
                        ;; Declarations
                        ,@(nth-value
                           1
                           (alexandria:parse-body (cddr g-defn)))
                        ;; Call to FLET function
                        (,(first g-defn) ,@(second g-defn))))
                   f-names
                   g-defns)))))

