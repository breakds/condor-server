;; ==================== Data structure ====================
(defstruct dispatcher name location pool)
(defstruct job status) ;; for status 0=pending -1=error 1=processing 2=complete 3=received

(defun make-vector ()
  "make a length-adjustable array (vector)"
  (make-array 0 :adjustable t))

(defun push-back (x vec)
  "push a new element to the back of a vector."
  (let ((old-len (length vec)))
    (adjust-array vec (1+ old-len))
    (setf (aref vec old-len) x)))
  


;; ==================== Internal Variables ====================
(defparameter *acceptor* nil "the condor dispatchers acceptor")
(defparameter *dispatchers* (make-hash-table :test #'equal) "the set of dispacthers")


;; ==================== External Variables ====================
(defparameter *log-path* "~/tmp/condor_server.log")


;; ==================== Aux Functions ===================
;; Log Facilities
(defparameter *months* #("Nil" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun time-stamp ()
  "get the time stamp printed to string"
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (format nil "[~2,'0d:~2,'0d:~2,'0d ~a-~a-~a]" hour minute second 
            (aref *months* month) day year)))

(defun log-to-file (type fmt &rest args)
  "log msg to a certain file, or *log-path* when filename is not provided."
  (with-open-file (*standard-output* *log-path*
                                     :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :append)
    (format t "~a " (time-stamp))
    (format t "[~a] " (cond ((eq type 'info) "info")
                             ((eq type 'error) "error")
                             ((eq type 'warning) "warn")
                             ((eq type 'done) " ok ")
                             (t "*")))
    (apply #'format t fmt args)
    (format t "~%")))

;; File Operations
(defun copy-files (from to)
  "copy files under diretory specified by \"from\" to directory
  \"to\". Directory will be created if not exist. Copy is overwrite
  enabled."
  (if (cl-fad:directory-exists-p from)
      (progn 
        (when (cl-fad:file-exists-p to)
          (cl-fad:delete-directory-and-files to :if-does-not-exist :ignore))
        ;; Create target directory
        (ensure-directories-exist (concatenate 'string to "/fake"))
        (walk-directory from :directories :breadth-first (lambda (x) 
                        
        
  
  
                                       
  
                               


;; ==================== Handlers ====================
;; +----------------------------------------
;; | Create Dispatcher:
;; | Input: Dispatcher's name (name) and location (location)
;; | Output: "ok" on successul call and "error" otherwise
;; +----------------------------------------
(hunchentoot:define-easy-handler (create-dispatcher :uri "/create") (name location)
  (setf (hunchentoot:content-type*) "text/plain")
  (if (eq (hunchentoot:request-method*) :GET)
      (format nil "Sorry, buddy, but /create do not offer a webpage mode.")
      (cond 
        ((null name) 
         (log-to-file 'error "create: name not provided.") 
         (format nil "error"))
        ((gethash name *dispatchers*) 
         (log-to-file 'error "create: dispacther ~a exists." name)
         (format nil "error"))
        (t (setf (gethash name *dispatchers*) (make-dispatcher :name name
                                                               :location location
                                                               :pool (make-vector)))
           (log-to-file 'done "create: dispatcher ~a created successfully." name)
           (format nil "ok")))))


;; +----------------------------------------
;; | Add-Job Dispatcher:
;; | Input: path to the job (path), name of dispatcher (name)
;; | Output: "ok" on successul call and "error" otherwise
;; +----------------------------------------
(hunchentoot:define-easy-handler (create-dispatcher :uri "/add") (path name)
  (setf (hunchentoot:content-type*) "text/plain")
  (if (eq (hunchentoot:request-method*) :GET)
      (format nil "Sorry, buddy, but /add do not offer a webpage mode.")
      (let ((dispatcher (gethash name *dispatchers*)))
      (cond (dispatcher 


        ((null name) (log-to-file 'error "add: dispatcher's name not provided.") 
             (format nil "error"))
            ((gethash name *dispatchers*) 
             


                                                                   

(defun start-server (&optional (port 8855))
  "Start the server with/without a specific port"
  (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor :port port))
  (hunchentoot:start *acceptor*)
  (format t "server started.~%"))

(defun stop-server ()
  "Stop the server"
  (hunchentoot:stop *acceptor*)
  (format t "server stopped.~%"))





