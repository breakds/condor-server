;;;; condor-server.asd
;;;; author: breakds

(asdf:defsystem #:condor-server
    :serial t
    :depends-on (#:hunchentoot)
    :components ((:file "lisp/package")
                 (:file "lisp/condor-server")))
                 
    
