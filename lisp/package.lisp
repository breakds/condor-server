;;;; package.lisp

(defpackage #:breakds.condor-server
  (:nicknames #:condor-server)
  (:use #:cl)
  (:export #:job
           #:dispatcher
           #:start-server
           #:stop-server))
  
