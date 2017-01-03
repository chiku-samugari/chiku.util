(in-package :chiku.util)

;;; Rainy lambda list is either a symbol or an extended lambda list,
;;; each of parameters is either a 1st order parameter or a 2nd order
;;; parameter. A 1st order parameter is a symbol, and a 2nd order
;;; parameter is a list of rainy-lambda-lists. Be aware that lambda
;;; keywords such as &REST is NOT parameters of an extended lamda list.
;;; They are lambda keywords.
;;;  Except special symbols called ``raindrop'', no symbol is allowed to
;;; appear multiple times in a rainy lambda list. Which symbols are the
;;; raindrop is decided in the application of Rainy lambda list.
;;;  A rainy lambda list is, from structural viewpoint, a variation of
;;; destructuring lambda list. These 2 extended lambda lists accepts
;;; same lambda keywords (&OPTIONAL, &REST, &KEY, &ALLOW-OTHER-KEYS, and
;;; &WHOLE). These differ in 2 aspects. First, it is not allowed to use
;;; a single dot (.) within rainy lambda list. Second, special symbols,
;;; raindrop, are allowed to appear multiple times. Parameters of a
;;; rainy lambda list are used as variables in some applications.
;;; Variables named by raindrops are called ``raindrop variables''.
;;;
;;;  I will use this rainy lambda list in DBIND and MVBIND to attach
;;; igrnored variables feature to DESTRUCTURING-BIND and
;;; MULTIPLE-VALUE-BIND. However, I name the structure not the function.
;;; Ignoring some variables is the function of DBIND, not the function
;;; of rainy lambda list. If I name a function, then one actual
;;; structure that is used to provide 2 different function must be given
;;; different names. It is useful in some cases, and ugly in some other
;;; cases. But these 2 naming policy is blended in the specification of
;;; lambda list family in Common Lisp.
(defun seal-raindrops (raindrop rainy-lambda-list)
  (let* (sealed-vars
         (lambda-list
           (mapleaf #'(if (eq a0 raindrop)
                        (car (push (gensym "SEALED")
                                   sealed-vars))
                        a0)
                    rainy-lambda-list)))
    (values lambda-list sealed-vars)))

(defun parameter1-p (param)
  "Checks if PARAM is a 1st order parameter."
  (symbolp param))

;;; Using a single symbol to LOW-LINE-LAMBDA-LIST is understood as
;;; &WHOLE parmeter.
;;;
;;;  (dbind a (list 1 2 3) a)
;;;    == (destructuring-bind (&whole a) (list 1 2 3))
;;;
;;; It justifies to made acceptable the single symbol in DBIND.
(defmacro dbind (low-line-lambda-list expression &body body)
  " A variation of DESTRUCTURING-BIND that adopts low-line lambda list
   instead of destructuring lambda list. Low-line lambda list is a rainy
   lambda list whose raindrop is AZUKI:_, a symbol in AZUKI package and
   whose name is composed of a single low-line (#\_).
    As well as DESTRUCTURING-BIND, DBIND recognizes symbols in
   LOW-LINE-LAMBDA-LIST as variables and binds them to the corresponding
   values in the tree structure returned from EXPRESSION. In addition,
   LOW-LINE-LAMBDA-LIST can be a single symbol and in that case a
   variable that is named by the symbol is bound to the whole value
   returned from EXPRESSION. Raindrop variables are DECLAREd as IGNORE
   (i.e. they are unavailable in BODY). BODY form  is evaluated under
   this variable bindings."
  (if (parameter1-p low-line-lambda-list)
    `(let ((,low-line-lambda-list ,expression))
       ,@body)
    (multiple-value-bind (lambda-list sealed-vars)
      (seal-raindrops '_ low-line-lambda-list)
      `(destructuring-bind ,lambda-list
         ,expression
         (declare (ignore ,@sealed-vars))
         ,@body))))

(defmacro mvbind (low-line-lambda-list1 expression &body body)
  " A variation of MULTIPLE-VALUE-BIND that adopts low-line lambda list1
   instead of the list of variables. MVBIND DECLAREs raindrop variables
   as IGNORE and thus, these variables are not available in BODY.
    Low-line lambda list1 is a Low-line lambda list which is composed of
   only symbols. tailing `1' means first order, a flat list of symbols."
  (multiple-value-bind (lambda-list sealed-vars)
    (seal-raindrops '_ low-line-lambda-list1)
    `(multiple-value-bind ,lambda-list
       ,expression
       (declare (ignore ,@sealed-vars))
       ,@body)))

;;; About the combination of MVBIND and DBIND, MBIND, be aware that only
;;; the first level of the low-line lambda list is to catch the multiple
;;; values. There provided no method to return a list that includes
;;; multiple values. From this reason, we can replace MVBIND but cannot
;;; replace DBIND.
;;;
;;;   (dbind (a _ x) expr body)
;;;    == (mbind ((a _ x)) expr body)
;;;
;;; We can use the combination of DBIND and MULTIPLE-VALUE-LIST for the
;;; same purpose. It is disadvantageous for perfoance because a list is
;;; constructed. But if we wants to avoid to use MULTIPLE-VALUE-LIST,
;;; then we have to give up to specify DECLARE for the variables.
(defun gen-dbind-stack (params low-line-lambda-lists body)
  (cond ((null params) body)
        ((parameter1-p (car low-line-lambda-lists))
         (gen-dbind-stack
           (cdr params) (cdr low-line-lambda-lists) body))
        (t `((dbind ,(car low-line-lambda-lists) ,(car params)
               ,@(gen-dbind-stack
                   (cdr params) (cdr low-line-lambda-lists) body))))))

(defun declare-used-p (body)
  (and (listp (car body))
       (eq (caar body) 'declare)))

(defun lambda-list1-p (rainy-lambda-list)
  (every #'parameter1-p rainy-lambda-list))

(defun cloak-parameter2 (low-line-lambda-list)
  "Cloaks 2nd order parameters by GENSYMs."
  (loop :for param :in low-line-lambda-list
        :for i = 0 :then (1+ i)
        :collect (if (parameter1-p param)
                   param
                   (gensym[] "2ND-PARAMETER" i))))

(defmacro mbind (low-line-lambda-list expression &body body)
  " MBIND, which stands for Multi Bind, is a combination of DBIND and
   MVBIND. EXPRESSION returns multiple values and each parameter of
   LOW-LINE-LAMBDA-LIST is bound to the respective returned value. 1st
   order parameters are bound to the returned value itself. 2nd order
   parameters are bound in the destructuring manner. In the case that
   LOW-LINE-LAMBDA-LIST is a symbol, then the variable whose name is the
   symbol is bound to the first value returned from EXPRESSION. Raindrop
   variables are DECLAREd as IGNORE and made unavailable in BODY. BODY
   form is evaluated under this variable bindings.
    Low-line lambda list is a rainy lambda list whose raindrop is
   AZUKI:_, a symbol in AZUKI package and whose name is composed of a
   single low-line (#\_).
    In order to support DECLARE specified by users, the combination of a
   single DBIND and MULTIPLE-VALUE-LIST is used when DECLARE is used at
   the top of BODY. It does not indebted by the multiple value in that
   situation. MVBIND can be an alternative if the destructuring feature
   is not needed."
  (cond ((parameter1-p low-line-lambda-list)
         `(let ((,low-line-lambda-list ,expression))
            ,@body))
        ((lambda-list1-p low-line-lambda-list)
         `(mvbind ,low-line-lambda-list ,expression
            ,@body))
        ((declare-used-p body)
         `(dbind ,low-line-lambda-list (multiple-value-list ,expression)
            ,@body))
        (t (let ((mvvars (cloak-parameter2 low-line-lambda-list)))
             `(mvbind ,mvvars ,expression
                ,@(gen-dbind-stack mvvars low-line-lambda-list body))))))
