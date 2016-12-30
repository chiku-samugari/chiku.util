(in-package :chiku.util)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun mapleaf (fn tree &optional (pred #'atom))
    "Walk on a tree that implemented as a multi-level list. All elements
     of TREE that returns non-NIL value when applied PRED are recognized
     as leaf nodes of this tree and applied the given function FN.
      A tree must be composed of inner nodes and leaf nodes. TREE is not
     allowed to include a portion that returns NIL when applied both of
     LISTP and the function given to PRED. MAPLEAF assumes such a node
     be an inner node and tires to pull out children. It means that
     MAPLEAF cannot be used to map over inner nodes. MAPBRANCH is for
     that purpose.
      PRED parameter is still effective since the leaf of a tree can be
     lists. It is not prohibited. Think of next list:

         '((0 1) (2 3) ((4 5)))

     We can use (lambda (_) (= (length _) 2)) as PRED and then we claim
     that all the lists whose length is 2 are leaf nodes. There is no
     conceptual error -- Each node is either an inner node or a leaf
     node."
    (if (funcall pred tree)
      (funcall fn tree)
      (mapcar (lambda (node) (mapleaf fn node pred)) tree))))

(defmacro with-tree-leaves (tree test-form result-form)
  `(mapleaf (lambda (leaf) (if ,test-form ,result-form leaf)) ,tree))

(defun mapbranch (picker fn tree &optional (leaf-proc #'identity) (leaf-pred #'atom))
    "Walk on a tree that implemented as a multi-level list. All elements that
     returns non-NIL value when applied LEAF-PRED are leaves and applied
     LEAF-PROC.In addition, elements that return non-NIL value when applied
     PICKER are applied the given function FN.
      If LEAF-PROC will never called, it might be better to adopt MAPLEAF.
     (MAPBRANCH #'FOO #'BAR TREE) is equal to (mapleaf #'BAR TREE #'FOO) in
     that case."
  (labels ((rec (node)
             (cond ((funcall leaf-pred node) (funcall leaf-proc node))
                   ((funcall picker node) (funcall fn node))
                   (t (mapcar #'rec node)))))
    (rec tree)))
