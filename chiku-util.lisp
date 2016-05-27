;;;; chiku-util.lisp : I defined some useful utilities in this file.
(in-package :chiku.util)

(proclaim '(inline last1 append1))

;;; WITH-GENSYMS macro
;;; I took it from "Practical Common Lisp".
;;; Oct. 09th 2013, chiku
;;; I want to pass an argument to GENSYM.
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop :for n :in names :collect
          (let ((name n))
            (if (atom name)
              `(,name (gensym ,(symbol-name name)))
              `(,(car name) (gensym (or ,(cadr name) "G"))))))
     ,@body))

(defmacro let-it-be (expr &body body)
  `(let ((it ,expr))
     ,@body
     it))

(defmacro aif (test-form then-form &optional else-form)
  (with-gensyms (tmp)
    `(let ((,tmp ,test-form))
       (if ,tmp
         (let ((it ,tmp))
           ,then-form)
         ,else-form))))

(defmacro aand (&rest forms)
  (cond ((null forms) t)
        ((null (cdr forms)) (car forms))
        (t `(aif ,(car forms) (aand ,@(cdr forms))))))

;;; LAST1 function
;;; I took this name from "On-Lisp".
(defun last1 (lst) (car (last lst)))

;;; APPPEND1
;;; I took this function from "On Lisp".
(defun append1 (lst obj) (append lst (list obj)))

;;; CHECK&STRING= function;{{{
;;; This function checks if all 2 arguments
;;; which are passed to this function are string before compare them.;}}}
(defun check&string= (x y) (and (stringp x) (stringp y) (string= x y)))

;;; NEXT-TO function;{{{
;;; This function returns object in the sequence "seq" next to "item".
;;; For list, same as following
;;; (cadr (member item lst));}}}
(defun next-to (item seq &key (test #'eql) (key #'identity))
  (let ((pos (position item seq :test test :key key)))
    (and pos (/= (1- (length seq)) pos) (nth (1+ pos) seq))))

;;; IOTA function
(defun iota (num &key (start 0) (step 1))
  (do ((i start (+ i step))
       (acc '() (cons i acc))
       (c 0 (1+ c)))
    ((= c num) (nreverse acc))))

;;; FILTER-NEXT-TO function;{{{
;;; Originally, the name of this function was DO-AND-SEARCH-MORE.
;;; I observe the behavior of this function and decide to change the name.
;;; This function takes at least 4 parameters and 2 keyword parameters.
;;; Please take care. The order of parameters have changed.
;;; Call the given function "fn" with the next element of "mark" in "lst".
;;; Repeat this action to the subsequence of "lst" starts with "restart-from";}}}
(defun filter-next-to (fn seq mark restart-from &key (test #'eql) (key #'identity))
  (when seq
    (let ((mark-pos (position mark seq :key key :test test))
          (restart-pos (position restart-from seq :key key :test test)))
      (and mark-pos (< mark-pos (length seq))
           (cons (funcall fn (elt seq (1+ mark-pos)))
                 (if restart-pos (filter-next-to fn (subseq seq (1+ restart-pos)) mark restart-from :key key :test test)))))))

;(filter-next-to #'identity "a * (b) + (c) + d + (e) - (f)" #\( #\) :test #'char=)
;;; Following is the original implementation of DO-AND-SEARCH-MORE function.;{{{
;;; The behavior is slight different from FILTER-NEXT-TO function.
;;; But FILTER-NEXT-TO is more natural. FILTER-NEXT-TO is more intuitive.
;(defun do-and-search-more (seq func start end &key (key #'identity) (test #'eql))
;  (when seq
;    (let ((spos (position start seq :key key :test test))
;          (epos (position end seq :key key :test test)))
;      (if (and spos epos (< spos epos))
;        (remove nil (list (funcall func (elt seq (1+ spos)))
;              (do-and-search-more (subseq seq (1+ epos)) func start end :key key :test test)))))))
;(do-and-search-more "a * (b) + (c) + d + (e) - (f)" #'identity #\( #\) :test #'char=);}}}

;;; SEQLAST1 function
;;; This function behaves same as LAST1 but works for any sequences.
(defun seqlast1 (seq) (elt seq (1- (length seq))))

;;; SEQBUTLAST function
;;; This function behaves same as BUTLAST but works for any sequences.
(defun seqbutlast (seq &optional (n 1)) (subseq seq 0 (- (length seq) n)))

;;; SEQLAST function
(defun seqlast (seq &optional (n 1))
  (subseq seq (- (length seq) n)))

;;; DROP function;{{{
;;; This function behaves same as NTHCDR but works for any sequences.
;;; In programming language Haskell, this function is called "drop"
;;; But if you don't specify "n", then it becomes same to "tail" in Haskell.;}}}
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun drop (seq &optional (n 1)) (subseq seq n)))

;;; TAKE function
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun take (seq &optional (n 1)) (subseq seq 0 n)))

;;; POSITION-LIST function;{{{
;;; This function makes the list constructs from the position of "x" in "lst".
;;; I implemented this one for my research
;;; but the main idea came from a book called "Programming Haskell" written by Graham Hutton.
;;; I implement this function based on POSITION (ex: the order of "item" and "lst")
;;; Current implementation has 2 problems.
;;; 1.) Current implementation works only for list.
;;;     Actually, this is a big problem because the library function "position" works for sequence.
;;; 2.) The performance is not so good.
;;;     The main problem is REMOVE function. I think this function can be defined
;;;     in tail-recursive form with accumulator and push-nreverse idiom.
;;; By the way, I feel the definition of "pred" inner function is quite good.
;;; I feel this is a proper usage of inner function.;}}}
;;;(defun position-list (item lst &key (key #'identity) (test #'eql))
;;;  (labels ((pred (x) (funcall test item x)))
;;;    (remove nil (mapcar (lambda (x idx) (if (pred (funcall key x)) idx nil)) lst (iota (length lst))))))
;;;
;;; 22 Dec, 2010 chiku
;;; I revised the definition. This version works not only for list but
;;; also for general sequence.
(defun position-list (item seq &key (key #'identity) (test #'eql))
  (labels ((rec (s acc)
                (let ((pos (position item s :key key :test test)))
                  (if pos
                    (rec (subseq s (1+ pos)) (cons (1+ pos) acc))
                    (nreverse (maplist (lambda (x) (reduce #'+ x)) acc))))))
    (rest (rec seq (list -1)))))

;;; POSITINO-IF-LIST function;{{{
;;; This function makes the list constructs from the position of
;;; elements in "lst" which satisfies "pred".
;;; 16th Nov, 2010
;;; This function must be implemented based on POSITION-IF.  Thus I
;;; change the order of "pred" and "lst". (I've already been familiar
;;; with this order, in any way);}}}
(defun position-if-list (pred seq &key (key #'identity))
  (if (eql (type-of seq) 'cons)
    (let ((dummy (gensym)))
      (remove dummy (mapcar (lambda (x idx)
                              (if (funcall pred (funcall key x))
                                idx
                                dummy))
                            seq (iota (length seq)))))
    (let (result)
      (dotimes (idx (length seq) (nreverse result))
        (if (funcall pred (funcall key (elt seq idx)))
          (push idx result))))))

;;; CONCAT-STR function;{{{
;;; This function will concatenate given strings.
;;; It's the short form for "(concatenate 'string ... )"
;;; Should it be written as a macro? I'm not sure ...;}}}
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun concat-str (&rest strs)
    (apply #'concatenate (cons 'string strs))))


;;; FLATTEN function;{{{
;;; I took this name and concept from OnLisp.
;;; Following is my implementation (different from implementation at OnLisp).
;(defun flatten (tree)
;  (labels ((rec (tree)
;                (cond ((null tree) nil)
;                      ((atom (car tree)) (cons (car tree) (rec (cdr tree))))
;                      (t (append (rec (car tree)) (rec (cdr tree)))))))
;    (rec tree)))
;;; I feel the implementation at OnLisp is better.
;;; It's not good to understand the behavior, but has better performance.;}}}
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun flatten (tree)
    (labels ((rec (tree acc)
               (cond ((null tree) acc)
                     ((atom tree) (cons tree acc))
                     (t (rec (car tree) (rec (cdr tree) acc))))))
      (rec tree nil))))

;;; ITOA function;{{{
;;; I took this name from C.
;;; (As you know, this function is not in standard C.)
;;; The behavior is same as itoa.;}}}
(defun ITOA (num)
  (prin1-to-string num))

;;; TAKEWHILE function
;;; I took this function from Haskell.
;;; How can I give the implementation for sequence?
;;; Jan. 06th 2014, chiku
;;; Updated to be applicable to sequence. The order of argument is also
;;; changed.
(defun takewhile (pred seq &rest args &key from-end (start 0) end key)
  (declare (ignore from-end start end key))
  (subseq seq start (apply #'position-if (complement pred) seq args)))

;;; Oct. 25th 2012, chiku
;;; Happy birthday!
;;; Dropwhile function.
;;; Jan. 06th 2014, chiku
;;; Updated to be applicable to sequence. The order of argument is also
;;; changed.
(defun dropwhile (pred seq &rest args &key from-end (start 0) end key)
  (declare (ignore from-end start end key))
  (aif (apply #'position-if (complement pred) seq args)
    (subseq seq it)))

;;; GROUP function;{{{
;;; I took this name and idea from OnLisp. How useful book is this ...
;;; Gatheres each "n" elements in "lst" and returns the list of these groups.
;;; Following is my first implementation. It's good to understand the behavior.
;;; Of course, it should be changed into tail-recursive function.
;(defun group (lst n)
;  (when lst
;    (cons (subseq lst 0 (min n (length lst))) (group (subseq lst (min n (length lst))) n))))
;;; Here is my second implementation.
;;; I took the error handling code from OnLisp.
;(defun group (lst n)
;  (labels ((rec (l a)
;                (if l
;                  (rec (subseq l (min n (length l))) (cons (subseq l 0 (min n (length l))) a))
;                  (nreverse a))))
;    (rec lst nil)))
;;; Finally, I adopted the code from OnLisp.
;;; Code in OnLisp checks 2 things simultaniously :
;;; if the length of "l" is enough or not and if l is null or not.
;;; (The key idear was CONSP function.)
;;; My code has an advantage that it can be used for general sequence (not only for list),
;;; So I leave the seconde implementation after change the name.
;;; (But, I feel it's not so common situation that this function will be used for any sequence other than list.);}}}
(defun group (lst n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (lst acc)
           (let ((rest (nthcdr n lst)))
             (if (consp rest)
               (rec rest (cons (subseq lst 0 n) acc))
               (nreverse (cons lst acc))))))
    (rec lst nil)))

;(group (list 1 2 3 4 6) 2)

;;; SEQGROUP function
;;; This function behaves same as GROUP, but works for any sequence.
(defun seqgroup (seq n)
  (labels ((rec (s a)
             (if (/= 0 (length s))
               (rec (subseq s (min n (length s)))
                    (cons (subseq s 0 (min n (length s))) a))
               (nreverse a))))
    (rec seq nil)))

;;; COMPOSE function;{{{
;;; When I wanted to accumulate the result of functions,
;;; I realized this can be expressed by function composition.
;;; This function takes one argument "fn-lst" that is constructed by
;;; one-argument functions. And returns a function (object) that will
;;; apply the first function in "fn-lst", next the second function in
;;; "fn-lst" and so on.
;;; Mar. 22nd 2014, chiku
;;; The order of functions is not proper.
;;; ;}}}
#+nil
(defun compose (fn-lst)
  (lambda (x) (reduce (lambda (a fn) (funcall fn a)) (cons x fn-lst))))

;;; May 13 2011, chiku
;;; And other functions, which were taken from OnLisp.
;;; MAP-> function
;;; This function is, to say, abstraction of
;;; ``mapping over iota.'' Actually, map0-n is that.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun map-> (fn initial test-fn succ-fn)
    (do ((obj initial (funcall succ-fn obj))
         (result ()))
      ((funcall test-fn obj) (nreverse result))
      (push (funcall fn obj) result)))

  ;;; MAPA-B function
  (defun mapa-b (fn a b &optional (step 1))
    (do ((i a (+ i step))
         (result ()))
      ((> i b) (nreverse result))
      (push (funcall fn i) result)))

  ;;; MAP0-n function
  (defun map0-n (fn n)
    (mapa-b fn 0 n))

  ;;; MAP1-n function
  (defun map1-n (fn n)
    (mapa-b fn 1 n)))

;;; show-hash-table function
;;; Why there is no tiny function to list up
;;; and show me the contents  of a hash table?
(defun show-hash-table (ht &optional (stream t))
  (format stream "~% key -> value ~%")
  (format stream   "--------------~%")
  (maphash
    (lambda (k v)
        (format stream "~a -> ~a~%" k v))
    ht))

;;; MAPTREE function;{{{
;;; Walk on tree that implemented as multi-level list.
;;; Each element of tree will be checked by the function "pred".
;;; All elements that will have passed this check will be recognized as leaf of this tree.
;;; Apply a function that will be offered as argument "func" to each leave of this tree.;}}}
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun maptree (func tree &key (pred #'atom))
    (if (funcall pred tree)
      (funcall func tree)
      (mapcar (lambda (x) (maptree func x :pred pred)) tree))))

(defmacro with-tree-leaves (tree test-form result-form)
  `(maptree (lambda (leaf) (if ,test-form ,result-form leaf)) ,tree))

;;; July 25th 2012, chiku
;;; Some macros from ``On Lisp.''
;;; I typed it during the implementation of LL(k) parser but finally
;;; it's not used. Thus, I decided to save them to this file.
(defmacro in (obj &rest choices)
  (with-gensyms (insym)
    `(let ((,insym ,obj))
       (or ,@(mapcar (lambda (c) `(eql ,insym ,c)) choices)))))

(defmacro inq (obj &rest choices)
  `(in ,obj ,@(mapcar (lambda (c) `',c) choices)))

(defmacro in-if (fn &rest choices)
  (with-gensyms (fn-var)
    `(let ((,fn-var ,fn))
       (or ,@(mapcar (lambda (c) `(funcall ,fn-var ,c)) choices)))))

(defmacro curry (fn)
  " curry fn => function;{{{
    fn : a function object
    function : a function object
    This macro generates the curried version of function object ``fn.''
    Actually, the generated function object is not fully curried because only the first
    argument of ``fn'' becames partial-applicable. The fully currying requires the arity,
    but this information seems not to be available for the user. Although we can write
    a function (or macro) which returns the arity, I don't implement it yet because this
    version is useful enough for me.
  ;}}}"
  (with-gensyms (x)
    `(lambda (,x)
       (papply ,fn ,x))))

(defmacro str-case (str &body key-value-lst)
  (with-gensyms (given-str)
    `(let ((,given-str ,str))
       (cond
         ,@(mapcar #'(let ((key (car a0)))
                       `(,(if (atom key)
                            `(string= ,given-str ,key)
                            `(in-if #`(string= ,',given-str ,a0) ,@key))
                          ,@(cdr a0)))
                   key-value-lst)))))

;;; Sep. 26th 2011, chiku
;;; stride-maplist
;;; Apr. 24th 2012, chiku
;;; Bugfix. If the argument ``lst'' inculdes NIL as its element,
;;; the iteration could end with remaining some elements because
;;; the previous version used (nth (1- stride) lst) value to decide
;;; if the itertion should continue or not.
;;; Apr. 26th 2012 chiku
;;; I've decided to change the order of the parameters because
;;; it should be able to take multiple list as MAPCAR family
;;; functions are. Of course, we can adopt both of an OPTIONAL parameter
;;; and a REST parameter. Although this approach is not so complicated,
;;; I still support the changing because the parameter that normally
;;; given should NOT be a OPTIONAL parameter. In thses functions' case,
;;; we are strongly recommended to use MAPCAR family function if the stride
;;; is fixed to 1.
;;; About the new order, I think following two are possible:
;;; 1.) stride fn lst 2.) fn stride lst
;;; Since I currently have no critical criteria about this point, I
;;; take 1.). 1.) seems more similar to the MAPCAR family's parameter order.
(defun stride-maplist (stride fn lst)
  " stride-maplist stride fn lst => list;{{{
    stride : a positive integer
    fn : a function object
    lst : a list

    The behavior of this function is similar to ``maplist'' function.
    The only difference is, this function skips n elements on each
    iteration. Here, Let n be the value of ``stride.''

    If stride is 1,  then the behavior is same to MAPLIST.

    ex.) (stride-maplist 2 #'car (iota 10)) -> (0 2 4 6 8)
  ;}}}"
  (labels ((rec (lst acc)
             (if (<= stride (length lst))
               (rec (drop lst stride) (cons (funcall fn lst) acc))
               (nreverse acc))))
    (rec lst nil)))

;;; Dec. 11th 2011, chiku
;;; STRIDE-MAPCON
;;; APPLY function could be REDUCE function. Which is better?
;;; I checked the performance by TIME macro and APPLY was better.
;;; When I come to think of it, it is natural. REDUCE function
;;; includes consing.
;;; Apr. 24th 2012, chiku
;;; Bugfix. See the comment for STRIDE-MAPLIST on the same day.
;;; Apr. 26th 2012, chiku
;;; Change the order of parameters. For more detail, please take a
;;; glance at the comment of MAPLIST.
(defun stride-mapcon (stride fn lst)
  " stride-mapcon stride fn lst => list;{{{
    stride : a positive integer
    fn : a function object
    lst : a list

    The behavior of this function is similar to ``MAPCON'' function.
    The only difference is, this function skips n elements on each
    iteration. Here, Let n be the value of ``stride.''

    If stride is 1, then the behavior is same to MAPCON.

    ex.) (stride-mapcon 2 #'copy-list (iota 4)) -> (0 1 2 3 2 3)
  ;}}}"
  (labels ((rec (lst acc)
             (if (<= stride (length lst))
               (rec (drop lst stride) (cons (funcall fn lst) acc))
               (apply #'nconc (nreverse acc)))))
    (rec lst nil)))

;;; Oct. 17th 2011, chiku
;;; STRIDE-MAPCAR function
;;; There are 2 choices about the behavior of this function.
;;; Here, I decided to pass CAR of lst par ``stride,'' to ``fn.''
;;; Another choice is, pass (take lst stride) to ``fn.''
;;; The latter choice can be achieved easily by following idiom:
;;; (mapcar fn (group lst stride))
;;; Thus, I think the latter one is not worth to be defined.
;;; Dec. 11th 2011, chiku
;;; The lattter choice is easily achieved by STRIDE-MAPLIST.
;;; Apr. 24th 2012, chiku
;;; Bugfix. See the comment for STRIDE-MAPLIST on the same day.
;;; Apr. 26th 2012, chiku
;;; Change the order of parameters. For more detail, please take a
;;; glance at the comment of MAPLIST.
(defun stride-mapcar (stride fn lst)
  (labels ((rec (lst acc)
             (if (<= stride (length lst))
               (rec (drop lst stride) (cons (funcall fn (car lst)) acc))
               (nreverse acc))))
    (rec lst nil)))

;;; Dec. 11th 2011, chiku
;;; STRIDE-MAPCAN function
;;; Apr. 24th 2012, chiku
;;; Bugfix. See the comment for STRIDE-MAPLIST on the same day.
;;; Apr. 26th 2012, chiku
;;; Change the order of parameters. For more detail, please take a
;;; glance at the comment of MAPLIST.
(defun stride-mapcan (stride fn lst)
  (labels ((rec (lst acc)
             (if (nth (1- stride) lst)
               (rec (drop lst stride) (cons (funcall fn (car lst)) acc))
               (apply #'nconc
                      (nreverse
                        (if (null lst)
                          acc
                          (cons (funcall fn (car lst)) acc)))))))
    (rec lst nil)))

;;; Apr. 26th 2012, chiku
;;; GROUP-FILTER function
;;; I want this function for filter a property list.
;;; Current version explains the algorithm pretty well, even though
;;; it might perform bad.
(defun group-filter (n fn lst)
  (filter (p (apply fn)) (group lst n)))

;;; Sep 30th 2011, chiku
;;; LISTFORK-IF function
(defun listfork-if (forkp lst)
  " listfork-if forkp lst => list of list ;{{{
    forkp : a function object (predicator)
    lst : list

    All the element in ``lst'' that returns T to ``forkp'' triggers
    the fork of copying ``lst.'' Let me call such element be
    ``fork-trigger.'' Both of the list which include the fork-trigger
    and doesn't include fork-trigger will be generated. The element
    which is not a fork-trigger, will just be included.
    As a result, each element of result list expresses one certain
    include-or-not pattern of fork-trigger.

    So to speak, the element of the result list will include ALL the
    leaves of the binary tree whose internal node is corresponding to
    each fork-trigger in ``lst.'' One branch is the branch which
    decides to include the fork-trigger, while the another branch
    decides to do not include the fork-trigger.

    ex.)
    (listfork-if #'symbolp (list 1 2 'a 3 'b))
    -> ((1 2 A 3 B) (1 2 3 B) (1 2 A 3) (1 2 3))
  ;}}}"
  (labels ((rec (lst)
             (if lst
               (mapcan (lambda (x)
                           (let ((head (car lst)))
                             (if (funcall forkp head)
                               (list (cons head x) x)
                               (list (cons head x)))))
                       (rec (cdr lst)))
               (list ()))))
    (rec lst)))

;;; The following version is the first version. Easier to understand I expect.
;(defun forklist (forkp lst)
;  (if lst
;    (mapcan (lambda (x)
;                (if (funcall forkp (car lst))
;                  (list (cons (car lst) x) x)
;                  (list (cons (car lst) x))))
;            (forklist forkp (cdr lst)))
;    (list ())))


;;; Oct. 8th 2011, chiku
;;; REMOVE-NEXT-TO function
;;; Dec. 19th 2011, chiku
;;; There must be a bug. The last element of ``lst'' is omitted.
(defun remove-next-to (mark lst &key (test #'eql) (keep-mark? t))
  (labels ((rec-keep-mark (lst acc)
             (if (and lst (cdr lst))
               (if (funcall test mark (car lst))
                 (rec-keep-mark (cddr lst) (cons (car lst) acc))
                 (rec-keep-mark (cdr lst) (cons (car lst) acc)))
               (nreverse (if (car lst) (cons (car lst) acc) acc))))
           (rec-remove-mark (lst acc)
             (if (and lst (cdr lst))
               (if (funcall test mark (car lst))
                 (rec-remove-mark (cddr lst) acc)
                 (rec-remove-mark (cdr lst) (cons (car lst) acc)))
               (nreverse (if (car lst) (cons (car lst) acc) acc)))))
    (if keep-mark?
      (rec-keep-mark lst nil)
      (rec-remove-mark lst nil))))


;;; Oct. 16th 2011, chiku
;;; MKLIST function
;;; I took it from ``On Lisp.''
;;; To tell the truth, I couldn't understand why this function is
;;; desired because I'm from the statical type system, and it's a
;;; evil idea, at least for me, one object can sometimes be an atom
;;; and somtimes a list. I still thinks it's bad idea, and everytime
;;; I use the idea of type whenever I write the program.
;;; However, there are many useful libraries and sometimes those
;;; libraries uses this idea, unfortunately. When I manage this kind
;;; of library, this MKLIST function is quite useful and desiable.
(defun mklist (obj)
  (if (listp obj) obj (list obj)))

;;; Nov. 4th 2011, chiku
;;; GROUP-HEADED function
;;; In the APTCC project recently, I introduced GROUP-BY-MARK function
;;; which divides a list into some lists based on a specified mark. I
;;; feel it useful and decide to register it in this library after
;;; changing the name of it to GROUP-HEADED. Additionally, I implemented
;;; the -IF version.
;;; Jan. 9th 2012, chiku
;;; Happy New Year!
;;; I've decided to change the name of parameter ``head-discard''
;;; because this name seems as if this parameter controled if the
;;; very first element that was not headed by the given mark
;;; (``head'' parameter) should be discarded or not. Thus, I decided
;;; tr change the names of parameter ``head'' to ``head-mark'' and
;;; ``head-dscard'' to ``discard-mark?'', respectively.
;;; Additionally, I decided to return the very first elements which
;;; is not headed by mark as a 2nd value of this function.
(defun group-headed (head-mark seq &key (mark-discard? nil) (key #'identity) (test #'eql))
  (let ((head-pos (position-list head-mark seq :key key :test test)))
    (values
      (mapcar
        (p (subseq seq))
        (if mark-discard? (mapcar #'1+ head-pos) head-pos)
        (drop (append1 head-pos (length seq))))
      (subseq seq 0 (car head-pos)))))

(defun group-headed-if (pred seq &key (mark-discard? nil) (key #'identity))
  (let ((head-pos (position-if-list pred seq :key key)))
    (values
      (mapcar
        (p (subseq seq))
        (if mark-discard? (mapcar #'1+ head-pos) head-pos)
        (drop (append1 head-pos (length seq))))
      (subseq seq 0 (car head-pos)))))

;;; Jan. 9th 2012, chiku
;;; Happy New Year!
;;; I've gotten an idea from above GROUP-HEADED function.
(defun group-tailed (tail-mark seq &key (mark-discard? nil) (key #'identity) (test #'eql))
  (let ((tail-pos (position-list tail-mark seq :key key :test test)))
    (values
      (mapcar
        (p (subseq seq))
        (cons 0 (mapcar #'1+ tail-pos))
        (if mark-discard?  tail-pos (mapcar #'1+ tail-pos)))
      (if tail-pos (subseq seq (1+ (last1 tail-pos))) seq))))

(defun group-tailed-if (pred seq &key (mark-discard? nil) (key #'identity))
  (let ((tail-pos (position-if-list pred seq :key key)))
    (values
      (mapcar
        (p (subseq seq))
        (cons 0 (mapcar #'1+ tail-pos))
        (if mark-discard?  tail-pos (mapcar #'1+ tail-pos)))
      (if tail-pos (subseq seq (1+ (last1 tail-pos))) seq))))

;;; FILTER function
;;; I took this function from "On Lisp"
;;; Aug. 27th 2013, chiku
;;;  Improve it to take 2 or more lists.
(defun filter (fn &rest lsts)
  (let ((acc nil))
    (dolist (args (apply #'mapcar #'list lsts) (nreverse acc))
        (aif (apply fn args)
          (push it acc)))))
;;; To tell the truth, what I used in my code was more limited version.;{{{
;;; I called this function COLLECT, and implementation was following.
;(defun collect (lst pred)
;  (when lst
;    (remove nil (mapcar (lambda (x) (if (funcall pred x) x nil)) lst))))
;;;; As you see, this function makes the list which only includes the elements from
;;; "lst" that passes "pred".
;;; This function can be expressed by FILTER, like this.
;(defun collect (lst pred) (filter (lambda (x) (if (funcall pred x) x nil)) lst))
;;; But please notice that this function is same as REMOVE-IF-NOT.
;(defun collect (lst pred) (remove-if-not pred lst))
;;; From this point of view, COLLECT has no worth to implent.
;;; However, REMOVE-IF-NOT function can't be used to implemnt FILTER. FILTER is worth to be implented.;}}}

;;; Jan. 19th 2012, chiku
;;; This is a lax function to access a text file.
(defun str<-textfile (filename &optional (omit-pred #'null))
  " The parameter ``omit-pred'' is used as a predicator for line-wise
    checking of taking that line or not."
  (with-open-file (in filename :direction :input)
    (when in
      (do ((line (read-line in nil nil) (read-line in nil nil))
           (str ""))
        ((null line) str)
        (unless (funcall omit-pred line)
          (setf str (concat-str str line (string #\newline))))))))

;;; Jan. 27th 2012, chiku
;;; AFTER function (from On Lisp) returns non-nil value in the case that,
;;; 1.) both of first and second arguments appear in the third argument
;;; 2.) the first argument appears AFTER the second argument, in the third argument.
;;; The return value is similar to that of MEMBER function.
(defun after (x y lst &key (key #'identity) (test #'eql) (x-not-before-y nil))
  (labels ((rec-y (lst)
             (when lst
               (if (funcall test y (funcall key (car lst)))
                 (rec-x (cdr lst))
                 (rec-y (cdr lst)))))
           (strict-rec-y (lst)
             (when (and lst (not (funcall test x (funcall key (car lst)))))
               (if (funcall test y (funcall key (car lst)))
                 (rec-x (cdr lst))
                 (strict-rec-y (cdr lst)))))
           (rec-x (lst)
             (when lst
               (if (funcall test x (funcall key (car lst)))
                 lst
                 (rec-x (cdr lst))))))
    (if x-not-before-y
      (strict-rec-y lst) (rec-y lst))))

;;; Apr. 27th 2012, chiku
;;; DENOTATIVE-SORT function
;;; During the trial of bibtex2csv, this function was desired.
;;; I feel this is quite interesting and only for that reason,
;;; I decided to register it to this library. Since this
;;; function requires the a concrete list that denotes order of
;;; elements, I name it ``denotative-sort.'' The word
;;; ``denotative'' is taken from the logic.
;;;
;;; I guess that it should take another parameter to control
;;; the ``seq's'' element that doesn't appear in ``order-lst.''
;;; Possible choises are discard, error, nil.
(defun denotative-sort (order-lst seq &key key test)
  " denotative-sort order-lst seq &key key test => sorted-seq ;{{{
    order-lst : a list
    seq : a sequence (this sequence will be destructively changed!)
    key : a designator for a function of one argument
    key : a designator for a function of two arguments
    sorted-seq : a sequence

    This function changes the position of each element in ``seq''
    by referring ``order-lst.'' The elements in the ``sorted-seq''
    are put in the same order to those of ``order-lst.''

    If the keyword parameter ``key'' is given, then the result of
    the ``key'' is used as sort keys of each element. If ``key'' is
    not given, then the element it self
    is used.

    In order to decide the position of each element of ``seq'' in
    ``order-lst,'' we have to check the equality between the element
    in these two lists. If the keyword parameter ``test'' is given,
    it is used for this purpose. EQL is the default.

    ex.)
        (denotative-sort '(2 1 0) (iota 3)) => (2 1 0) ; boring
        (denotative-sort '(0 1 2) (iota 10) :key (papply #'mod _ 3))
            => (0 3 6 9 1 4 7 2 5 8)
  ;}}}"
  (sort
    seq
    (lambda (x y)
        (< (position x order-lst :test test)
           (position y order-lst :test test)))
    :key key))

(defun denotative-stable-sort (order-lst seq &key key test)
  " denotative-stable-sort order-lst seq &key key test => sorted-seq
    A stable variation of denotative-sort. See the denotative-sort.
  "
  (stable-sort
    seq
    (lambda (x y) (< (position x order-lst :test test) (position y order-lst :test test)))
    :key key))

;;; May 7th 2012, chiku
;;; SCAN function
;;; This function sometimes called ``prefix sum.''
;;; June 3rd 2012, chiku
;;; I've decided to change the name of this function
;;; to PREFIX-SUM.
(defun prefix-sum (num-lst)
  (nreverse (maplist (p #'reduce #'+) (reverse num-lst))))

;;; May 7th 2012, chiku
;;; ZIP function
;;; This function is taken from the programming language Haskell.
;;; This kind of function makes us easy to scratch a first code.
;;; Aug 6th 2012, chiku
;;; I've decided to change the definition. In Haskell, zip function
;;; returns a list of 2-tuple. When I used this function in one of
;;; my toy projects, I first assumed this property. Thus, I decided
;;; to change the return value to a list of dot pairs. Extension of
;;; ZIP function that can take 3 or more lists is not needed so much.
;;; Even if it's needed, it's quite easy to implement.
;;; Nov. 5th 2012, chiku
;;; Did you know? It is also appears in CLtL2.
(defun zip (seq1 seq2)
  (map 'cons #'cons seq1 seq2))

;;; May 10th 2012, chiku
;;; YIELD function
;;; Like SOME function, YIELD function returns the first non-nil
;;; value that is generated by the function ``gen-fn'' by applying it
;;; to each element of ``seq'' from the first element to the last.
;;; The difference between them is the way of decision making about
;;; return value.
;;; In a manner of speaking, SOME function uses IDENTITY function
;;; for generating the value to deside if the value can be the return
;;; value or not. YIELD function uses the given ``check-key'' function
;;; to decide it. From this view point, it is similar to FIND or other
;;; sequence functions. Or, in other words, this function is SOME
;;; function with KEY keyword parameter.
;;; The ``check-key'' parameter is required parameter. If you don't
;;; need it, then SOME function is more proper.
(defun yield (gen-fn check-key seq &rest more-seqs)
  (labels ((rec (lst)
             (if lst
               (let ((val (funcall gen-fn (car lst))))
                 (if (funcall check-key val)
                   val (rec (cdr lst))))))
           (rec-lsts (lsts)
             (if (not (member nil lsts))
               (let ((val (apply gen-fn (mapcar #'car lsts))))
                 (if (funcall check-key val)
                   val (rec-lsts (mapcar #'cdr lsts))))))
           (rec-seqs (seqs)
             (if (not (find 0 seqs :key #'length))
               (let ((val (apply gen-fn (mapcar (p (elt _ 0)) seqs))))
                 (if (funcall check-key val)
                   val (rec-seqs (mapcar #'drop seqs)))))))
    (if (eql (type-of seq) 'cons)
      (if more-seqs
        (rec-lsts (cons seq more-seqs))
        (rec seq))
      (rec-seqs (cons seq more-seqs)))))
;;; Following is the first implementation.
;;; (defun yield (fn check-key lst)
;;;   (labels ((rec (lst)
;;;              (if lst
;;;                (let ((val (funcall fn (car lst))))
;;;                  (if (funcall check-key val)
;;;                    val
;;;                    (rec (cdr lst)))))))
;;;     (rec lst)))

;;; May 10th 2012, chiku
;;; CHECK function
;;; It returns the given object ``obj'' when it returns non-nil
;;; value in the evaluation with ``pred.'' It seems really easy function,
;;; but it can give the KEY keyword parameter of sequence functions for
;;; any function! I don't have to waste the time for thinking a proper
;;; variable name in LET to hold the name for checking anymore.
(defun check (pred obj)
  (if (funcall pred obj) obj))

;; May 10th 2012, chiku
;; INT-COMPOSITIONS function
(defun int-compositions (n &key (len) (parts-max n) (parts-min 1))
  (labels ((rec  (n max lst)
             (cond ((= n 0) lst)
                   ((= max 1) (mapcar (p (cons n)) lst))
                   (t (mapcan
                        (lambda (i) (rec (- n i) (1- max) (mapcar (p (cons i)) lst)))
                        (iota n :start 1))))))
    (let ((parts-max (if len len parts-max)) (parts-min (if len len parts-min)))
      (remove-if-not (lambda (x) (<= parts-min (length x)))
        (rec n parts-max (list ()))))))

;;; May 10th 2012, chiku
;;; MAP-INT-COMPOSITIONS function
(defun map-int-compositions (n fn &key (len) (parts-max n) (parts-min 1))
  (mapcar fn (int-compositions n :len len :parts-max parts-max :parts-min parts-min)))

(defun >case-cl-expand (v cl)
  (let ((key (car cl)) (val (cdr cl)))
    (cond ((consp key) `((in ,v ,@key) ,@val))
          ((inq key t otherwise) `(t ,@val))
          (t (error "Malformed >CASE clause.
                     CAR of CLAUSES must be a list of keys or key generating forms.
                     : ~a" cl)))))

(defmacro >case (expr &body clauses)
  (with-gensyms (v)
    `(let ((,v ,expr))
       (cond ,@(mapcar (p (>case-cl-expand v)) clauses)))))

;;; July 30th 2012, chiku
;;; It's ported from LL(k) project.
(defun flatten-weakly (lsts)
  (apply #'append lsts))

;;; Aug 5th 2012, chiku
;;; MAPPEND from ANSI Common Lisp (a book).
;;; After such a long time, I realized
;;; that MAPCAN is dangerous in some scenes.
(defun mappend (fn lst &rest more-lsts)
  (flatten-weakly (apply #'mapcar fn lst more-lsts)))

;;; Aug 9th 2012, chiku
;;; WHILE, TILL and AWHILE macro
(defmacro while (test &body body)
  `(do ()
     ((not ,test))
     ,@body))

(defmacro till (test &body body)
  `(do ()
     (,test)
     ,@body))

(defmacro awhile (form &body body)
  `(do ((it ,form ,form))
     ((not it))
     ,@body))

;;; Aug 30th 2012, chiku
;;; ONCE-ONLY macro
;;; My own implementation. Slightly different from the Practical
;;; Common Lisp version. About the discussion, Please look at
;;; ~/src/lisp/once-only/main.lisp.
;(defmacro once-only ((&rest vars) &body body)
;  (let ((gensyms (mapcar (papply #'list (gensym)) vars)))
;    `(let ,gensyms
;       (with-gensyms ,vars
;         `(let (,,@(mapcar (lambda (x) `(list ,@(reverse x))) gensyms))
;            ,,@body)))))
;;; Here is the Practical Common Lisp version.
;;; Dec. 4th 2012, chiku
;;; I realized that the result for MACROEXPAND-1 is different. PCL version
;;; is more clearer.
;;; Nov. 20th 2014, chiku
;;; The name of GENSYMs are improved.
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop :for n :in names :collect (gensym (concat-str "SUB-" (string n))))))
    `(let (,@(loop :for g :in gensyms :for n :in names :collect `(,g (gensym ,(string n)))))
       `(let (,,@(loop :for g :in gensyms :for n :in names :collect ``(,,g ,,n)))
          ,(let (,@(loop :for n :in names :for g :in gensyms :collect `(,n ,g)))
             ,@body)))))

;;; Oct. 7th 2012, chiku
;;; ALAMBDA from On Lisp
(defmacro alambda ((&rest args) &body body)
  `(labels ((self ,args
              ,@body))
     #'self))

;;; Oct. 7th 2012, chiku
;;; KEYMAX function
;;; Apr. 23rd 2014, chiku
;;; In Lisp Meet Up 16, I came to know that this idea is called argmax
;;; in mathematics.
(defun argmax (key &rest args)
  "MAX function accompanied with a function that act as a :KEY keyword
   parameter in some sequence functions."
  (if args
    (find (apply #'max (mapcar key args)) args :key key)))

(defun argmin (key &rest args)
  (if args
    (find (apply #'min (mapcar key args)) args :key key)))

;;; Oct. 7th 2012, chiku
;;; A variation of LONGER function from On Lisp.  The definition
;;; of LONGER is very smart and beautiful. On the other hand, when
;;; it comes to the use, SHORTER is preferlable.  What I'm saying
;;; is just a tiny thing: < is easy to image than >. Thus, I
;;; wanted the SHORTER function. However, SHORTER is IMPOSSIBLE to
;;; be defined in the same approach.
;;;  In SHORTER's case, we need to confirm if Y is null or not
;;; independently of the same check for X, vice versa. Inversely,
;;; SHORTER-OR-EQUAL is possible to be implemented with this
;;; beautiful approach, while LONGER-OR-EQUAL is impossible.
;;;  There is another possible set of names for this function:
;;; LEN< or LENGTH<. I think it's a very good name, but the
;;; built-in < function can take arbitrary number of arguments.
;;; I think LEN< (or LENGTH<) should obey this convention.
(defun shorter (seq0 seq1)
  "Returns T if and only if ``SEQ0' is shorter than ``SEQ1.''"
  (labels ((longer (x y)
             (and (consp x)
                  (or (null y) (longer (cdr x) (cdr y))))))
    (if (and (listp seq0) (listp seq1))
      (longer seq1 seq0)
      (< (length seq0) (length seq1)))))

;;; Jul. 15th 2013, chiku
;;; SHORTER-OR-EQUAL is required during the implementation of
;;; rdiff.
(defun shorter-or-equal (seq0 seq1)
  "Returns T if the length of ``SEQ0'' is shorter or equals
   that of ``SEQ1''"
  (labels ((impl-for-list (x y)
             (or (null x)
                 (and (not (null y)) (impl-for-list (cdr x) (cdr y))))))
    (if (and (listp seq0) (listp seq1))
      (impl-for-list seq0 seq1))))

;;; DLAMBDA macro from Let Over Lambda
;;; I don't need default method.
;;; Jan. 16th 2013, chiku
;;; I was wrong. It seems convinient to have the default method.
(defmacro dlambda (&body keyword-args-body)
  (with-gensyms (args)
  `(lambda (&rest ,args)
       (case (car ,args)
         ,@(mapcar
             (lambda (x)
                 `(,(if (eq (car x) t)
                      t
                      (list (car x)))
                    (apply (lambda ,@(cdr x))
                           ,(if (eq (car x) t)
                              args
                              `(cdr ,args)))))
             keyword-args-body)))))

;;; LREC function and ALREC macro
;;; From On Lisp.
(defun lrec (rec &optional base)
  (alambda (lst)
    (if (null lst)
      (if (functionp base)
        (funcall base)
        base)
      (funcall rec (car lst) (lambda () (self (cdr lst)))))))

(defmacro alrec (recform &optional base)
  (with-gensyms (fn)
                `(lrec (lambda (it ,fn)
                         (declare (ignorable it ,fn))
                         (symbol-macrolet ((rec (funcall ,fn)))
                           ,recform))
                       ,base)))

;;; Oct. 16th 2012, chiku
;;; PRINTING-LET and PRINTING-LET* macro
;;; For scratching process.
;;; PRITING- macro is generalizing the idea of these macros.
;;; WRAP-BODYFORMS generalize the idea of PRINTING- macro.
(defmacro wrap-bodyforms (op skip &body form)
  `(,@(take (car form) skip)
     ,@(mapcar (lambda (form)
                 (if (eq (and (consp form) (car form)) 'declare) form (list op form)))
               (drop (car form) skip))))

(defmacro printing- (skip &body form)
  `(wrap-bodyforms print ,skip ,(car form)))

(defmacro printing-let ((&rest bindings) &body body)
  `(printing- 2 (let ,bindings ,@body)))

(defmacro printing-let* ((&rest bindings) &body body)
  `(printing- 2 (let* ,bindings ,@body)))

;;; Oct. 28th 2012, chiku
;;; CONDLET macro from On Lisp
;;; I cannot understand why the argument of BODYFN have
;;; to be GENSYMs.
(defun extract-vars (clauses)
  (mapcar (lambda (var) (cons var (gensym (symbol-name var))))
          (remove-duplicates (mapcar #'car (mappend #'cadr clauses)))))

(defun condlet-clauses-expand (clauses vars bodyfn)
  `(cond ,@(mapcar (lambda (cl)
                    `(,(car cl) (let ,(expand-binds (cadr cl) vars)
                                  (,bodyfn ,@(mapcar #'cdr vars)))))
                  clauses)))

(defun expand-binds (binds vars)
  (mapcar (lambda (var)
            (aif (find (car var) binds :key #'car)
              (cons (cdr var) (cdr it))
              (cdr var)))
            vars))

(defmacro condlet (clauses &body body)
  (with-gensyms (bodyfn)
    (let ((vars (extract-vars clauses)))
      `(labels ((,bodyfn ,(mapcar #'car vars) ,@body))
         ,(condlet-clauses-expand clauses vars bodyfn)))))

;;; Oct. 28th 2012, chiku
;;; >CASELET macro. My original.
(defmacro >caselet (keyform clauses &body body)
  (with-gensyms (key)
    (let ((modified-clauses
            (mapcar (lambda (cl)
                      (destructuring-bind (test-forms binds) cl
                        (cond ((consp test-forms) `((in ,key ,@test-forms) ,binds))
                              ((inq test-forms t otherwise) `(t ,binds)))))
                    clauses)))
      `(let ((,key ,keyform))
         (condlet ,modified-clauses ,@body)))))

;;; Jan. 2nd 2013, chiku
;;; ACOND from On Lisp.
(defmacro acond (&rest clauses)
  (if (car clauses)
    (destructuring-bind (test &rest forms) (car clauses)
      (if (or (eq t test) (eq 'otherwise test))
        `(progn ,@forms)
        (with-gensyms (save)
          `(let ((,save ,test))
             (if ,save
               (let ((it ,save)) (declare (ignorable it)) ,@forms)
               (acond ,@(cdr clauses)))))))))

;;; Apr. 06th 2013, chiku
;;; WRAP-IF
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun search-protected-code (protect-point-symbol code)
    (when (consp code)
      (some
        (lambda (x)
          (if (and (consp x) (eq protect-point-symbol (car x)))
            x (search-protected-code protect-point-symbol x)))
        code))))

(defmacro wrap-if (test &body form)
  (with-gensyms (x)
    `(macrolet ((requisite (,x) ,x))
       (if ,test
         (progn ,@form)
         (progn ,(search-protected-code 'requisite form))))))

;;; Jul. 15th 2013, chiku
;;; WRAP-UNLESS
(defmacro wrap-unless (test form)
  `(wrap-if (not ,test) ,form))

;;; May. 04th 2013, chiku
;;; We cannot implement a function that passes a symbol type
;;; to WITH-PACKAGE-ITERATOR macro because  SYMBOL-TYPE
;;; argument will never evaluated. Instead, I provide a macro
;;; named PACKAGE-SYMBOL-LIST that returns a list of specified
;;; symbol-type from the specified package, and other 3
;;; functions for each specific symbol type.
(defmacro package-symbol-list (pkgdsg symbol-type)
  `(with-package-iterator (next ,pkgdsg ,symbol-type)
    (let (sym-lst)
      (loop
        (multiple-value-bind (more? s accessibility package)
          (next)
          (declare (ignorable accessibility package))
          (if more?
            (push s sym-lst)
            (return sym-lst)))))))

(defun package-internal-symbols (pkgdsg)
  (package-symbol-list pkgdsg :internal))

(defun package-external-symbols (pkgdsg)
  (package-symbol-list pkgdsg :external))

(defun package-inherited-symbols (pkgdsg)
  (package-symbol-list pkgdsg :inherited))

;;; May. 04th 2013, chiku
;;; I'm not sure this is good way or even a proper way, but recently
;;; I use this code many times. When we find out a better way, it
;;; should be fixed immediately.
;;; Mar. 11st 2015, chiku
;;;  It does not work properly if pathdsg was not tailed with a
;;; directory separator. This function should recognize any inputs as
;;; directory names.
(defun change-directory (pathdsg)
  (let ((pname (pathname pathdsg)))
    (setf *default-pathname-defaults*
          (make-pathname
            :directory (append (pathname-directory *default-pathname-defaults*)
                               (aif (pathname-directory pname)
                                 (drop it))
                               (aand (pathname-name pname) (list it)))))))

;;; Aug. 27th 2013, chiku
;;; DOLISTS macro.
(labels ((collect-vars (forms)
           (mapcar #'car forms))
         (lookup-initform (var vars-and-lists)
           (cadr (assoc var vars-and-lists))))
  (defmacro dolists (vars-and-lists &body body)
    (let ((vars (mapcar #'(cons a0 (gensym (concat-str (symbol-name a0) "-LST")))
                        (collect-vars vars-and-lists))))
      `(symbol-macrolet (,@(mapcar #`(,(car a0) (car ,(cdr a0))) vars))
         (do (,@(mapcar #`(,(cdr a0) ,(lookup-initform (car a0) vars-and-lists) (cdr ,(cdr a0))) vars))
           ((in-if #'null ,@(mapcar #'cdr vars)) nil)
           ,@body)))))

;;; Oct. 06th 2013, chiku
;;; A limited variation of MAPHASH
(defun maphash-value (fn hash-table)
  (maphash (lambda (k v)
             (declare (ignore k))
             (funcall fn v))
           hash-table))

(defun maphash-key (fn hash-table)
  (maphash (lambda (k v)
             (declare (ignore v))
             (funcall fn k))
           hash-table))

;;; Nov. 14th 2013, chiku
;;; LISTING function and LIST/DET macro
;;; See src/lisp/listing-with-determiner/ for detail.
(defun listing (&rest det-item-pairs)
  " det-item-pairs : a list of pairs whose CAR determines if the
    CDR element should be included in the returned list."
  (reduce (lambda (pair partial)
            (if (car pair)
              (cons (cdr pair) partial)
              partial))
          det-item-pairs
          :initial-value ()
          :from-end t))

(defmacro list/det% (&body clauses) 
  (if clauses
    (destructuring-bind ((det val) . tl) clauses
      (if (eq det t) ; Not just DET. I want to limit this optimization.
        `(cons ,val (list/det% ,@tl))
        `(if ,det
           (cons ,val (list/det% ,@tl))
           (list/det% ,@tl))))))

(defmacro list/det (&body clauses)
  (if (< (length clauses) 10)
    `(list/det% ,@clauses)
    `(listing ,@(mapcar #`(cons ,@a0) clauses))))

;;; Nov. 29th 2013, chiku
;;; PUSHUPDATE macro.
(defmacro pushupdate (obj place &key test key)
  " obj --- an object.
    place --- a place, the value of which is a proper list.
    test --- a designator for a function of two parameters that returns a
             generalized boolean.
    key --- a designator for a function of one parameter.

   It first removes all the objects that is equal to OBJ in PLACE and then
   PUSHes the OBJ into PLACE. Equality of objects are checked with the value
   that is acquired by applying the function specified to KEY keyword parameter
   and tested by the function specified to TEST keyword parameter. Defaults for
   KEY and TEST are #'IDENTITY and #'EQL, respectively.
    The KEY function, if supplied, is applied to OBJ too and the result object
   is used to the equality check, likewise PUSHNEW and ADJOIN.
   "
  (once-only (obj)
    `(progn
       (setf ,place
             (remove
               ,(if key `(funcall ,key ,obj) obj)
               ,place
               ,@(flatten-weakly
                   (list/det (key `(:key ,key))
                             (test `(:test ,test))))))
       (push ,obj ,place))))

;;; Nov. 29th 2013, chiku
;;; Collective variants of MAPHASH and its limited versions.
(defun cmaphash (fn hash-table)
  (let (result)
    (maphash (lambda (k v)
               (push (funcall fn k v) result))
             hash-table)
    (nreverse result)))

(defun cmaphash-value (fn hash-table)
  (let (result)
    (maphash-value (lambda (v)
                     (push (funcall fn v) result))
                   hash-table)
    (nreverse result)))

(defun cmaphash-key (fn hash-table)
  (let (result)
    (maphash-key (lambda (k)
                   (push (funcall fn k) result))
                 hash-table)
    (nreverse result)))

(defun fillin (lst len &optional fillin-value)
  (append lst (make-list (- len (length lst)) :initial-element fillin-value)))

(defmacro mvidentity (&rest multiple-values)
  `(multiple-value-call #'values ,@multiple-values))

(defun mvconstantly (&rest args)
  (lambda (&rest garbages)
    (declare (ignore garbages))
    (apply #'values args)))

(defun cons* (&rest args)
  " A variation of LIST*. LIST* returns the given object when it
   receives only one object. CONS* returns a list that includes the
   given object in that case. CONS* returns NIL if no arguments are
   given. For other cases, the behavior is same as that of LIST*."
  (if (< (length args) 2)
    args
    (apply #'list* args)))

(defun keywordsymbolize (sym)
  " KEYWORDSYMBOLIZE interns a symbol that will have the same name as
   the given symbol SYM into keyword package."
  (intern (symbol-name sym) :keyword))

(defun conc-intern (&rest strdsg-lst)
  " CONC-INTERN interns a symbol whose name is the concatenation of all
   the string designators in STRDSG-LST. String designators are first
   stringified by CL:STRING function. The symbol is interned to the
   current package."
  (intern (apply #'concat-str (mapcar #'string strdsg-lst))))

(defmacro splicing-tunnel (form)
  " The control if the returnd value from FORM should be treated as a
   single argument to LIST or multiple argument to LIST is transferred
   to FORM."
  `(multiple-value-list (mvidentity ,@form)))
