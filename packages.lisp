(defpackage :chiku.util
  (:use :cl :papply :azuki)
  (:export
    :map-int-compositions
    ;:extract-vars
    :stride-mapcan
    :group-filter
    ;:lexically-bound-p
    :inq
    :flatten-weakly
    :printing-
    :show-hash-table
    :filter
    :mklist
    ;:condlet-clauses-expand
    :group-tailed
    :filter-next-to
    :zip
    :once-only
    :dlambda
    :shorter
    :printing-let
    :iota
    :curry
    :last1
    :aand
    :wrap-if
    :dolists
    :package-internal-symbols
    :awhile
    :group
    :acond
    :condlet
    :yield
    :dropwhile
    :group-headed
    ;:compose
    :maphash-value
    :maphash-key
    :cmaphash
    :cmaphash-value
    :cmaphash-key
    ;:expand-binds
    :alrec
    :seqlast
    :after
    :while
    :>caselet
    :package-symbol-list
    :seqlast1
    :anaphorap
    :remove-next-to
    :package-external-symbols
    :mappend
    :map->
    :stride-mapcon
    :takewhile
    :stride-mapcar
    :append1
    :shorter-or-equal
    :wrap-unless
    :till
    :mapa-b
    :drop
    :check&string=
    :wrap-bodyforms
    :lrec
    :group-headed-if
    :str<-textfile
    ;:search-protected-code
    :concat-str
    :concat
    :package-inherited-symbols
    :with-tree-leaves
    :leaf
    :take
    :str-case
    :denotative-stable-sort
    :position-list
    :stride-maplist
    :group-tailed-if
    :flatten
    :aif
    ;:>case-cl-expand
    :printing-let*
    :in
    :alambda
    :with-gensyms
    :prefix-sum
    :next-to
    :map1-n
    :change-directory
    :denotative-sort
    :itoa
    :mapleaf
    :mapbranch
    :listfork-if
    :argmax
    :argmin
    :int-compositions
    :map0-n
    :position-if-list
    :check
    :seqgroup
    ;:anaphora-list
    :>case
    :in-if
    :seqbutlast
    :listing
    :list/det
    :pushupdate
    :fillin
    :mvidentity
    :mvconstantly
    :mvdo
    :mvdo*
    :mvpsetq
    :cons*
    :keywordsymbolize
    :conc-intern
    :splicing-tunnel
    :let-it-be
    :xstringify
    :gensym[]
    :dbind
    :mvbind
    :mbind
    :tprint
    :maximize
    :imapcar
    :with-functions
    :projected-find-if
    :maximals
    :seqmaximals
    ;; Variables
    :self :it :_ :requisite :rec))
