(defsystem :chiku.util
  :version "0.6.0"
  :maintainer "Takehiko Nawata"
  :author "Takehiko Nawata"
  :license "MIT License"
  :description "Utility"
  :long-description "A collection of utilities."
  :serial t
  :depends-on (:azuki :papply)
  :components ((:file "packages")
               (:file "map-tree")
               (:file "chiku-util")
               (:file "dbind")
               (:file "mvdo")))
