;;;; defrec.asd
;;;; Copyright (c) 2013 Robert Smith

(asdf:defsystem #:defrec
  :serial t
  :description "A library for defining top-level mutually recursive functions."
  :author "Robert Smith <quad@symbo1ics.com>"
  :license "BSD 3-clause"
  :components ((:file "package")
               (:file "defrec"))
  :depends-on (#:alexandria))

