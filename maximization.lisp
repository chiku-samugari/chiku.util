(in-package :chiku.util)

(defun argmax (key &rest args)
  "MAX function accompanied with a function that act as a :KEY keyword
   parameter in some sequence functions."
  (if args
    (find (apply #'max (mapcar key args)) args :key key)))

(defun argmin (key &rest args)
  (if args
    (find (apply #'min (mapcar key args)) args :key key)))

(defun maximize (seq order &optional (key #'identity))
  "The extended version of argmax (in math) which a function, ORDER, to
   decide the order of projected value is specifiable. Although KEY is
   not needed from the principall view point, it contributes for the
   clearer code in some cases. CL:SORT is used as the reference to
   decide the order of parameters.

   The ORDER function should be a total order, at least if that is
   restricted to SEQ. Even if there is a maximum element of SEQ,
   MAXIMIZE may not work as it is intended if ORDER function is not a
   total order since the order of comparison is not ruled at all.
    From the binary relational viewpoint, MAXIMIZE function returns the
   rightmost element of SEQ in the sence of ORDER. It goes without
   saying that the 2nd argument of ORDER function (among its 2
   arguments) is the right argument of the binary relation ORDER.

   An error will be raised if the length of SEQ is zero."
  (reduce #'(if (funcall order (funcall key a0) (funcall key a1)) a1 a0) seq))

;;; Since Common Lisp does not have generalized version of CONS, it is
;;; troublesome to return a sequence that is same type as SEQ with this
;;; algorithm.
;;;  We can gain the performance by keeping the set of projected items
;;; along with the actual result. I do not like that kind of
;;; performance. Easy to be done, hard to understand in the future.
(defun maximals (seq order &optional (key #'identity))
  "A list composed of maximal elements of SEQ in the sence of ORDER is
   returned. ORDER must be a partial order over SEQ; it must be
   reflexive, antisymmetric and transitive."
  (with-oneish (order key)
    (reduce (lambda (acc item)
              (let ((projected (key item)))
                (if (find-if #'(order projected (key _)) acc)
                  acc
                  ;; Be aware that to delete only the found element is
                  ;; enough because ACC is always an antichain.
                  (aif (projected-find-if #'(order _ projected) key acc)
                    (cons item (delete it acc :key key :test order))
                    (cons item acc)))))
            seq :initial-value ())))

(defun seqmaximals (seq order &optional (key #'identity))
  "A preferable variation of MAXIMALS for sequences. A sequence of same
   type to SEQ composed of SEQ's maximal elements in the sence of ORDER
   is returned. ORDER must be a partial order over SEQ; it must be
   reflexive, antisymmetric and transitive."
  (with-oneish (order key)
    ;; KEY of REMOVE-IF and FIND-IF is not useful in this case because
    ;; we want to check and skip if A0 is ELEMENT.
    (remove-if (lambda (element)
                 (let ((projected (key element)))
                   (find-if #'(and (not (eq element a0))
                                   (order projected (key a0)))
                            seq)))
               seq)))
