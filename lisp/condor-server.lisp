;; ==================== Data structure ====================
(defstruct dispatcher name location pool)
(defmethod dispatcher-last-job (d)
  "get the last job of the current dispatcher"
  (get-back (dispatcher-pool d)))

(defmethod dispatcher-next-pending-job (d)
  "return the id of the next pending job"
  (let ((pending-job (find-if (lambda (j) (= (job-status j) 0))
                              (dispatcher-pool d))))
    (when pending-job
      (job-id pending-job))))

(defmethod dispatcher-clear-pool (d)
  "clear the job pool of dispatcher"
  (setf (dispatcher-pool d) (make-vector)))

(defmethod dispatcher-set-status (d jobid st)
  "set the status of the specified job to be pending"
  (cond ((eq st 'pending) (setf (job-status (aref (dispatcher-pool d) jobid)) 0))
        ((eq st 'processing) (setf (job-status (aref (dispatcher-pool d) jobid)) 1))
        ((eq st 'complete) (setf (job-status (aref (dispatcher-pool d) jobid)) 2))
        ((eq st 'received) (setf (job-status (aref (dispatcher-pool d) jobid)) 3))
        (t (setf (job-status (aref (dispatcher-pool d) jobid)) -1))))

    

(defstruct job id status ip port) ;; for status 0=pending -1=error 1=processing 2=complete 3=received



(defun make-vector ()
  "make a length-adjustable array (vector)"
  (make-array 0 :adjustable t))

(defun push-back (x vec)
  "push a new element to the back of a vector."
  (let ((old-len (length vec)))
    (adjust-array vec (1+ old-len))
    (setf (aref vec old-len) x)))

(defun get-back (vec)
  "get the last element out of a vector"
  (aref vec (1- (length vec))))

  


;; ==================== Internal Variables ====================
(defparameter *acceptor* nil "the condor dispatchers acceptor")
(defparameter *dispatchers* (make-hash-table :test #'equal) "the set of dispacthers")
(defparameter *gui-tmpl* #P"../template/gui.tmpl")

;; ==================== External Variables ====================
(defparameter *log-path* "~/tmp/condor_server.log")
;; the trailing "/" is very important in *server-base*
(defparameter *server-base* #P"~/tmp/serverbase/")


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

;; Response Facilities
(defun signal-ok () "ok")
(defun signal-error () "error")



;; File Operations
(defun copy-files (from to)
  "copy files under diretory specified by \"from\" to directory
  \"to\". Directory will be created if not exist. Copy is overwrite
  enabled. Directory structure will be flatten in the destination"
  (when (cl-fad:directory-exists-p from)
    (progn 
      (when (cl-fad:file-exists-p to)
        (cl-fad:delete-directory-and-files to :if-does-not-exist :ignore))
      ;; Create target directory
      (cl-fad:walk-directory from 
                             (lambda (x) (let* ((filename (file-namestring x))
                                                (tobe (merge-pathnames filename to)))
                                           (when filename
                                             (ensure-directories-exist tobe)
                                             (cl-fad:copy-file x tobe))))))
    t))

(defun copy-file (from to)
  "copy a single file"
  (ensure-directories-exist to)
  (cl-fad:copy-file from to :overwrite t)
  t)

(defun pathname-fullname (path)
  "get the full file name from a file path"
  (concatenate 'string (pathname-name path) "." (pathname-type path)))


                        
;; Template Generation/Fill
(defun gen-gui-row (job-obj)
  "generate a row for one job in html"
  (list :job-id (format nil "~a" (job-id job-obj))
        :row-id (format nil "~a" (job-id job-obj))
        :status (cond ((= (job-status job-obj) 0) "pending")
                      ((= (job-status job-obj) 1) "processing")
                      ((= (job-status job-obj) 2) "complete")
                      ((= (job-status job-obj) 3) "received")
                      ((= (job-status job-obj) -1) "error"))))

(defun gen-gui-html (dispatcher-obj)
  "generate html file from template for one dispatcher"
  (with-output-to-string (html-template:*default-template-output*)
    (html-template:fill-and-print-template 
     *gui-tmpl*
     (list :row-num (length (dispatcher-pool dispatcher-obj)) 
           :rows (loop for item across (dispatcher-pool dispatcher-obj)
                    collect (gen-gui-row item))))))
        
  
  
                                       
  
                               


;; ==================== Handlers ====================
;; +----------------------------------------
;; | Create Dispatcher:
;; | Input: Dispatcher's name (name)
;; | Output: "ok" on successul call and "error" otherwise
;; +----------------------------------------
(hunchentoot:define-easy-handler (create-dispatcher :uri "/create") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (if (eq (hunchentoot:request-method*) :GET)
      (format nil "Sorry, buddy, but /create do not offer a webpage mode.")
      (cond 
        ((null name) 
         (log-to-file 'error "create: name not provided.") 
         (signal-error))
        ((gethash name *dispatchers*) 
         (log-to-file 'error "create: dispacther ~a exists." name)
         (signal-error))
        (t (setf (gethash name *dispatchers*) 
                 (make-dispatcher :name name
                                  :location (merge-pathnames (format nil "~a/" name)
                                                             *server-base*)
                                  :pool (make-vector)))
           (log-to-file 'done "create: dispatcher ~a created successfully." name)
           (log-to-file 'info "create: dispatcher ~a located at ~a." name 
                        (dispatcher-location (gethash name *dispatchers*)))
           (signal-ok)))))


;; +----------------------------------------
;; | Add-Job to Dispatcher:
;; | Input: path to the job (path), name of dispatcher (name)
;; | Output: "ok" upon successul call and "error" otherwise
;; +----------------------------------------
(hunchentoot:define-easy-handler (add-job :uri "/add") (path name)
  (setf (hunchentoot:content-type*) "text/plain")
  (if (eq (hunchentoot:request-method*) :GET)
      (format nil "Sorry, buddy, but /add do not offer a webpage mode.")
      (let ((object (gethash name *dispatchers*)))
        (cond (object (push-back (make-job :status 0 :id (length (dispatcher-pool object)))
                                 (dispatcher-pool object))
                      ;; copy job-specific input files
                      (let* ((job-obj (dispatcher-last-job object))
                             (destination (merge-pathnames (format nil "input/~a/" 
                                                                   (job-id job-obj))
                                                           (dispatcher-location object))))
                        (ensure-directories-exist (merge-pathnames "fake" destination))
                        (copy-files path destination))
                      (log-to-file 'done "add: ~a received a job from ~a, now have ~a job."
                                   name path (length (dispatcher-pool object)))
                      (signal-ok))
              (t (log-to-file 'error "add: dispatcher *~a* does not exist." name)
                 (signal-error))))))


;; +----------------------------------------
;; | Web Browser GUI
;; | Input: name of dispatcher
;; | Output: the gui webpage
;; +----------------------------------------
(hunchentoot:define-easy-handler (show-gui :uri "/gui") (name)
  (setf (hunchentoot:content-type*) "text/html")
  (let ((object (gethash name *dispatchers*)))
    (cond (object (log-to-file 'done "gui: gui request completed.")
                  (gen-gui-html object))
          (t (log-to-file 'error "gui: dispatcher *~a* does not exist." name)
             (signal-error)))))


;; +----------------------------------------
;; | Register slot (machine)
;; | Input: name of dispatcher
;; | Output: shared file list with a leading "ok" upon 
;; |         successul call and "error" otherwise
;; +----------------------------------------
(hunchentoot:define-easy-handler (register :uri "/register") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((d (gethash name *dispatchers*)))
    (if (null d)
        (progn
          ;; dispatcher not found
          (log-to-file 'error "register: dispatcher *~a* does not exist." name)
          (signal-error))
        (let* ((local-path (merge-pathnames "shared"
                                            (dispatcher-location d)))
               (file-list (cl-fad:list-directory local-path)))
          (log-to-file 'done "register: slot registered.")
          (format nil "ok~%~{~a~%~}"
                  (loop for file in file-list
                     collect (pathname-fullname file)))))))





;; +----------------------------------------
;; | Fetch Job
;; | Input: name of dispatcher
;; | Output: job id and filelist
;; +----------------------------------------
(hunchentoot:define-easy-handler (fetch-job :uri "/fetch") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((d (gethash name *dispatchers*)))
    (if (null d)
        (progn
          ;; dispatcher not found
          (log-to-file 'error "fetch: dispatcher *~a* does not exist." name)
          (signal-error))
        (let ((jobid (dispatcher-next-pending-job d)))
          (if jobid
              (let* ((local-path (merge-pathnames (format nil "input/~a/" jobid)
                                                  (dispatcher-location d)))
                     (file-list (cl-fad:list-directory local-path)))
                (log-to-file 'done "fetch: offering files for job *~a*:~a." name jobid)
                (dispatcher-set-status d jobid 'processing)
                (format nil "~a~%~{~a~%~}"
                        jobid
                        (loop for file in file-list
                           collect (pathname-fullname file))))
              (progn
                ;; no more pending jobs
                (log-to-file 'info "fetch: dispatcher *~a* no more jobs to offer." name)
                (format nil "-1")))))))


;; +----------------------------------------
;; | Reset dispatcher and will add every job
;; |     under *server-base*/<dispatcher name>/input
;; | Input: dispatcher name
;; | Output: "ok" on successul call and "error" otherwise
;; +----------------------------------------
(hunchentoot:define-easy-handler (reset-handler :uri "/reset") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((d (gethash name *dispatchers*)))
    (if (null d)
        (progn
          ;; dispatcher not found
          (log-to-file 'error "reset: dispatcher *~a* does not exist." name)
          (signal-error))
        (progn
          (dispatcher-clear-pool d)
          (loop for i from 0 
             while (cl-fad:directory-exists-p (merge-pathnames (format nil "input/~a/" i)
                                                               (dispatcher-location d)))
             do
               (push-back (make-job :status 0 :id (length (dispatcher-pool d)))
                          (dispatcher-pool d))
               (log-to-file 'done "reset: dispatcher *~a* picked a job, now have ~a jobs."
                            name  (length (dispatcher-pool d)))
          (signal-ok))))))


;; +----------------------------------------
;; | Signal completion of job
;; | Input: dispatcher name, job id
;; | Output: "ok" upon successul call and "error" otherwise
;; +----------------------------------------
(hunchentoot:define-easy-handler (sig-complete :uri "/sigcomplete") (name jobid)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((d (gethash name *dispatchers*)))
    (if (null d)
        (progn
          ;; dispatcher not founds
          (log-to-file 'error "sigcomplete: dispatcher *~a* does not exist." name)
          (signal-error))
        (if (<= (length (dispatcher-pool d)) (parse-integer jobid))
            (progn
              ;; jobid has not been created yet
              (log-to-file 'error "sigcomplete: dispatcher *~a* does not spawn job ~a."
                           name jobid)
              (signal-error))
            (progn
              (dispatcher-set-status d (parse-integer jobid) 'complete)
              (log-to-file 'done "sigcomplete: job *~a*:~a complete!" name jobid)
              (signal-ok))))))
                        


;; +----------------------------------------
;; | Handling upload request
;; | Input: dispatcher name, job id, the multipart form
;; | Output: "ok" on successul call and "error" otherwise
;; +----------------------------------------
(hunchentoot:define-easy-handler (upload-handler :uri "/upload") (name jobid)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((d (gethash name *dispatchers*)))
    (if (null d)
        (progn
          ;; dispatcher not founds
          (log-to-file 'error "upload: dispatcher *~a* does not exist." name)
          (signal-error))
        (if (<= (length (dispatcher-pool d)) (parse-integer jobid))
            (progn
              ;; jobid has not been created yet
              (log-to-file 'error "upload: dispatcher *~a* does not spawn job ~a."
                           name jobid)
              (signal-error))
            (let ((post-data (hunchentoot:post-parameter "data")))
              (if (null post-data)
                  (progn
                    ;; post-data not exist
                    (log-to-file 'error "upload: *~a*:~a, post-data does not exist."
                                 name jobid)
                    (signal-error))
                  (let ((path (first post-data)))
                    (if (copy-file 
                         path 
                         (merge-pathnames (format nil "output/~a.tar.gz" jobid)
                                          (dispatcher-location d)))
                        (progn
                          ;; successful
                          (log-to-file 'done "upload: received file for job *~a*:~a!"
                                       name jobid)
                          (dispatcher-set-status d (parse-integer jobid) 'received)
                          (signal-ok))
                        (progn 
                          ;; copy is not successful
                          (log-to-file 'error "upload: upload failed for job *~a*:~a."
                                       name jobid)
                          (signal-error))))))))))
                     
      


;; ==================== server-side controllers ====================

(defun start-server (&optional (port 8855))
  "Start the server with/without a specific port"
  (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor :port port
                                  :document-root *server-base*))
  (hunchentoot:start *acceptor*)
  (format t "server started.~%"))

(defun stop-server ()
  "Stop the server"
  (setf *dispatchers* (make-hash-table :test #'equal))
  (hunchentoot:stop *acceptor*)
  (format t "server stopped.~%"))





