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
       (progn ,@body))))
