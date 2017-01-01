(in-package :chiku.util)

(defun proc-sealed-lambda-list (sealed-var sealed-lambda-list)
  (let* (sealed-vars
         (lambda-list
           (mapleaf #'(if (eq a0 sealed-var)
                        (car (push (gensym "SEALED")
                                   sealed-vars))
                        a0)
                    sealed-lambda-list)))
    (values lambda-list sealed-vars)))

(defmacro dbind (low-line-lambda-list expression &body body)
  "A variation of DESTRUCTURING-BIND that adopts low-line lambda list
   instead of destructuring lambda list. Low-line lambda list is mostly
   same as destructuring lambda list excepts it treats all variables
   whose name is _, a symbol whose name is composed of a signle
   low-line, as sealed variables. Sealed variables are all DECLAREd as
   IGNOREd."
  (multiple-value-bind (lambda-list sealed-vars)
    (proc-sealed-lambda-list '_ low-line-lambda-list)
    `(destructuring-bind ,lambda-list
       ,expression
       (declare (ignore ,@sealed-vars))
       ,@body)))

(defmacro mvbind1 (low-line-lambda-list1 expression &body body)
  "A variation of MULTIPLE-VALUE-BIND that adopts low-line lambda list1
   instead of the list of variables. Low-line lambda list1 is a list of
   variables but a special symbol _ can be used multiple times and all
   of them are not available in the BODY form."
  (multiple-value-bind (lambda-list sealed-vars)
    (proc-sealed-lambda-list '_ low-line-lambda-list1)
    `(multiple-value-bind ,lambda-list
       ,expression
       (declare (ignore ,@sealed-vars))
       ,@body)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun gen-dbind-stack (mvvars low-line-lambda-lists body)
    (cond ((null mvvars) body)
          ((eq (car mvvars) '_)
           (gen-dbind-stack
             (cdr mvvars) (cdr low-line-lambda-lists) body))
          (t `((dbind ,(car low-line-lambda-lists) ,(car mvvars)
                ,@(gen-dbind-stack
                    (cdr mvvars) (cdr low-line-lambda-lists) body))))))

  (defun declare-used-p (low-line-lambda-list body)
    (and (listp (car body))
         (eq  (caar body)'declare)
         (not (every #'symbolp low-line-lambda-list)))))

(defmacro mvbind (low-line-lambda-list expression &body body)
  "A variation of MULTIPLE-VALUE-BIND and DESTRUCTURING-BIND. Value
   skipping feature is also available. Low-line lambda list is mostly
   same as destructuring lambda list excepts it treats all variables
   whose name is _, a symbol whose name is composed of a signle
   low-line, as sealed variables. Sealed variables are all DECLAREd as
   IGNOREd.
    In order to support DECLARE specified by users, the combination of a
   single DBIND and MULTIPLE-VALUE-LIST is used when DECLARE is used.
   MVBIND1 can be an alternative if the destructuring feature is not
   needed."
  (if (declare-used-p low-line-lambda-list body)
    `(dbind ,low-line-lambda-list (multiple-value-list ,expression)
       ,@body)
    (let ((mvvars (loop :for var :in low-line-lambda-list
                        :for i = 0 :then (1+ i)
                        :collect (if (eq var '_)
                                   '_
                                   (gensym[] "MVVAR" i)))))
      `(mvbind1 ,mvvars ,expression
         ,@(gen-dbind-stack mvvars low-line-lambda-list body)))))
