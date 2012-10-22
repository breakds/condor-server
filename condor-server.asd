;;;; condor-server.asd
;;;; author: breakds

(asdf:defsystem #:condor-server
    :description "condor-server: a web server providing interactive
    interface for manipulating condor jobs."
    :version "0.5.1"
    :author "BreakDS <breakds@gmail.com>"
    :license "Public Domain"
    :depends-on (#:hunchentoot
                 #:html-template)
    :serial t
    :components ((:file "lisp/package")
                 (:file "lisp/condor-server")))
                 
    
