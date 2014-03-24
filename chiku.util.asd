(defpackage :chiku.util.asd
  (:use :cl :asdf))

(in-package :chiku.util.asd)

(defsystem :chiku.util
  :version "0.6.0"
  :maintainer "Takehiko Nawata"
  :author "Takehiko Nawata"
  :license "MIT License"
  :description "Utility"
  :long-description "A collection of utilities."
  :serial t
  :components ((:file "packages")
               (:file "chiku-util")))
