(in-package :chiku.util)

;;; MVDO and MVDO* from On Lisp.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun singlevar-decl-p (vardecl)
    (symbolp (name-form vardecl)))

  (defun name-form (vardecl)
    (car vardecl))

  (defun init-form (vardecl)
    (second vardecl))

  (defun step-form (vardecl)
    (third vardecl))

  (defun has-step-form-p (vardecl)
    (eql (length vardecl) 3))
  (defun varinit-forms (varlist body)
    (if varlist
      (if (singlevar-decl-p (car varlist))
        `(let* ,(mapcar #'(if (eql (length a0) 1)
                            (name-form a0)
                            (take a0 2))
                        (takewhile #'singlevar-decl-p varlist))
           ,(varinit-forms (dropwhile #'singlevar-decl-p varlist) body))
        `(multiple-value-bind ,(name-form (car varlist))
           ,(init-form (car varlist))
           ,(varinit-forms (cdr varlist) body)))
      body))

  (defun construct-update-forms* (varlist)
    (if varlist
      (let ((top (car varlist)))
        (if (singlevar-decl-p top)
          (cons
            `(setq ,@(mapcan #'(list (name-form a0) (step-form a0))
                             (remove-if-not
                               #'has-step-form-p
                               (takewhile #'singlevar-decl-p varlist))))
            (construct-update-forms* (dropwhile #'singlevar-decl-p varlist)))
          (wrap-if (has-step-form-p top)
            (cons `(multiple-value-setq ,(name-form top) ,(step-form top))
                  (requisite (construct-update-forms* (cdr varlist)))))))))

  (defun expand-mvdo* (varlist endlist body)
    (with-gensyms (bodytag testtag)
      (varinit-forms
        varlist
        `(tagbody
           (go ,testtag)
           ,bodytag ,@body
           ,@(construct-update-forms* varlist)
           ,testtag
           (unless ,(car endlist) (go ,bodytag))
           (return (progn ,(second endlist))))))))

(defmacro mvdo* (varlist endlist &body body)
  `(block nil
     ,(expand-mvdo* varlist endlist body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun construct-update-forms (varlist)
    `(mvpsetq ,@(reduce #'(if (has-step-form-p a0)
                            (list* (car a0) (third a0) a1)
                            a1)
                        varlist :from-end t :initial-value ())))

  (defun expand-mvdo (varlist endlist body)
    (with-gensyms (bodytag testtag)
      (varinit-forms
        varlist
        `(tagbody
           (go ,testtag)
           ,bodytag ,@body
           ,(construct-update-forms varlist)
           ,testtag
           (unless ,(car endlist) (go ,bodytag))
           (return (progn ,(second endlist))))))))

(defmacro mvpsetq (&rest var-value-lst)
  (let ((tmpvars (stride-mapcar
                   2
                   (lambda (var)
                     (if (consp var)
                       (mapcar #'gensym[] var)
                       (gensym[] var)))
                   var-value-lst)))
    (labels ((rec (vars update-forms)
               (if vars
                 (let ((var (car vars))
                       (update-form (car update-forms)))
                   (if (consp var)
                     `(multiple-value-bind ,var
                        ,update-form
                        ,(rec (cdr vars) (cdr update-forms)))
                     `(let ((,var ,update-form))
                        ,(rec (cdr vars) (cdr update-forms)))))
                 `(setq ,@(mapcan #'list
                                  (flatten (stride-mapcar 2 #'identity var-value-lst))
                                  (flatten tmpvars))))))
      (rec tmpvars (stride-maplist 2 #'second var-value-lst)))))

(defmacro mvdo (varlist endlist &body body)
  `(block nil
     ,(expand-mvdo varlist endlist body)))
