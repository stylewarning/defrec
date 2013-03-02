;;;; defrec.lisp
;;;; Copyright (c) 2013 Robert Smith

(in-package #:defrec)

;;;; XXX: This code is really ugly. Could use a facelift sometime.

;;;; FIXME: Doesn't handle &foo arguments.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun &-symbol (symbol)
    (let ((posn (position #\& (symbol-name symbol))))
      (and posn (zerop posn)))))

(defmacro defrec (&body definitions)
  "Define mutually recursive functions. Each definition of DEFINITIONS
  should be of the same form of FLET or LABELS:

  (<function name> <lambda list>
    <function body>).

Currently, lambda lists can only be lists of symbols. &REST,
&OPTIONAL, and &KEY arguments are not supported."
  (assert (every (lambda (defn)
                   (and (listp defn)
                        (<= 2 (length defn))
                        (symbolp (first defn))
                        (listp (second defn))
                        (every #'symbolp (second defn))
                        (notany #'&-symbol (second defn))))
                 definitions)
          ()
          "Every DEFREC definition must have the form ~
           (symbol lambda-list body...) where the lambda list is a list of ~
           non-& symbols.")
  (when (null definitions)
    (return-from defrec nil))
  (cond
    ((null definitions) nil)
    ((null (cdr definitions)) (cons 'cl:defun (first definitions)))
    (t
     (let* ((f-names (mapcar #'first definitions))
            (decls   (mapcar (lambda (defn)
                               (nth-value
                                1
                                (alexandria:parse-body (cddr defn))))
                             definitions))
            (g-names (mapcar (lambda (f-name)
                               (gensym (symbol-name f-name)))
                             f-names))
            (g-defns (mapcar (lambda (g defn)
                               (cons g (cdr defn)))
                             g-names
                             definitions)))
       `(labels (,@definitions)
          (flet ,(mapcar (lambda (g-name f-name decl lam-list)
                           `(,g-name ,lam-list
                                     ,@decl
                                     (,f-name ,@lam-list)))
                  g-names
                  f-names
                  decls
                  (mapcar #'second definitions))
            
            (declare (inline ,@g-names))
            ,@(mapcar (lambda (f-name decl g-defn)
                        `(cl:defun ,f-name ,(second g-defn)
                           ,@decl
                           (,(first g-defn) ,@(second g-defn))))
                      f-names
                      decls
                      g-defns))
          ',f-names)))))
