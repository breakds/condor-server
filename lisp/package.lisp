;;;; package.lisp

(defpackage #:breakds.condor-server
  (:nicknames #:condor-server)
  (:use #:cl)
  (:export #:get-author
           #:job
           #:dispatcher
           #:start-server
           #:stop-server))
  
