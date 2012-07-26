;; ==================== Data structure ====================
(defstruct job id status (node-id -1) (ip "") ;; for status 0=pending -1=error 1=processing 2=complete 3=received
           (start-time-stamp 0) ;; start-time-stamp records a universal time
           (completion-time-stamp 0)
           (latest-update 0))

(defstruct dispatcher name location pool node-map nodes)
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

(defmethod dispatcher-clear-nodes (d)
  "clear the nodes information"
  (setf (dispatcher-nodes d) (make-vector))
  (setf (dispatcher-node-map d) (make-hash-table :test #'equal)))

(defmethod dispatcher-clear-reports (d)
  "delete all the report files"
  (cl-fad:delete-directory-and-files (merge-pathnames "report"
                                                      (dispatcher-location d))
                                                      :if-does-not-exist :ignore))

(defmethod dispatcher-count-pending-jobs (d)
  "counting pending jobs"
  (loop for j across (dispatcher-pool d)
     counting (= 0 (job-status j))))

(defmethod dispatcher-count-jobs (d)
  "counting jobs"
  (length (dispatcher-pool d)))



(defmethod dispatcher-init (d)
  "initialization of dispatcher"
  (dispatcher-clear-reports d)
  (dispatcher-clear-pool d)
  (dispatcher-clear-nodes d))

(defmethod dispatcher-set-status (d jobid st)
  "set the status of the specified job to be pending"
  (cond ((eq st 'pending) (setf (job-status (aref (dispatcher-pool d) jobid)) 0))
        ((eq st 'processing) (setf (job-status (aref (dispatcher-pool d) jobid)) 1))
        ((eq st 'complete) (setf (job-status (aref (dispatcher-pool d) jobid)) 2
                                 (job-completion-time-stamp (aref (dispatcher-pool d) jobid)) (get-universal-time)))
        ((eq st 'received) (setf (job-status (aref (dispatcher-pool d) jobid)) 3))
        (t (setf (job-status (aref (dispatcher-pool d) jobid)) -1
                 (job-completion-time-stamp (aref (dispatcher-pool d) jobid)) (get-universal-time)))))

    





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
(defparameter *view-tmpl* #P"../template/console.tmpl")
(defparameter *interface-tmpl* #P"../template/interface.tmpl")

;; ==================== External Variables ====================
(defparameter *log-path* "/scratch.1/breakds/condor/base/log/condor_server.log")
;; the trailing "/" is very important in *server-base*
(defparameter *server-base* #P"/scratch.1/breakds/condor/base/")


;; ==================== Aux Functions ===================
;; Log Facilities
(defparameter *months* #("Nil" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun time-stamp (&optional (utime (get-universal-time)))
  "get the time stamp printed to string"
  (multiple-value-bind (second minute hour day month year) (decode-universal-time utime)
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
                                             (cl-fad:copy-file x tobe :overwrite t))))))
    t))

(defun copy-file (from to)
  "copy a single file"
  (ensure-directories-exist to)
  (cl-fad:copy-file from to :overwrite t)
  t)

(defun pathname-fullname (path)
  "get the full file name from a file path"
  (if (pathname-type path)
      (concatenate 'string (pathname-name path) "." (pathname-type path))
      (pathname-name path)))
  

;; string parser
(defun split-integers (str &optional (delim #\|))
  "Parse integer list in delimiter-separated string."
  (labels ((split-integers-iter (s accu)
             (let ((i (position delim s)))
               (format t "~a" i)
               (if i
                   (split-integers-iter (subseq s (1+ i))
                                        (cons (parse-integer (subseq s 0 i))
                                              accu))
                   (reverse (cons (parse-integer s) accu))))))
    (split-integers-iter str nil)))
        

                        
;; Template Generation/Fill
(defun decode-universal-time-diff (tic toc)
  "decode time difference between universal time stamp into string"
  (let* ((diff (- toc tic)))
    (multiple-value-bind (day diff-1) (floor diff 86400)
      (multiple-value-bind (hour diff-2) (floor diff-1 3600)
        (multiple-value-bind (minute second) (floor diff-2 60)
          (cond ((= 1 day) (format nil "1 day, ~2,'0d:~2,'0d:~2,'0d" hour minute second))
                ((< 1 day) (format nil "~a days, ~2,'0d:~2,'0d:~2,'0d" day hour minute second))
                (t (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second))))))))

    
(defun gen-gui-row (dispatcher-obj job-obj)
  "generate a row for one job in html"
  (list :job-id (format nil "~a" (job-id job-obj))
        :row-id (format nil "~a" (job-id job-obj))
        :ip (job-ip job-obj)
        :start-time (if (= (job-start-time-stamp job-obj) 0)
                        ""
                        (time-stamp (job-start-time-stamp job-obj)))
        :duration (cond ((= (job-start-time-stamp job-obj) 0) "")
                        ((or (>= (job-status job-obj) 2) (= (job-status job-obj) -1))
                         (decode-universal-time-diff (job-start-time-stamp job-obj)
                                                     (job-completion-time-stamp job-obj)))
                        (t (decode-universal-time-diff (job-start-time-stamp job-obj)
                                                       (get-universal-time))))
        :report-url (when (or (>= (job-status job-obj) 1) (= (job-status job-obj) -1))
                        (format nil "view?name=~a&jobid=~a" (dispatcher-name dispatcher-obj)
                                (job-id job-obj)))
        :status (cond ((= (job-status job-obj) 0) "pending")
                      ((= (job-status job-obj) 1) "processing")
                      ((= (job-status job-obj) 2) "complete")
                      ((= (job-status job-obj) 3) "received")
                      ((= (job-status job-obj) -1) "error"))
        :status-color (cond ((= (job-status job-obj) 0) "silver")
                            ((= (job-status job-obj) 1) "blue")
                            ((= (job-status job-obj) 2) "lime")
                            ((= (job-status job-obj) 3) "fuchsia")
                            ((= (job-status job-obj) -1) "red"))))

(defun gen-gui-html (dispatcher-obj)
  "generate gui html file from template for one dispatcher"
  (with-output-to-string (html-template:*default-template-output*)
    (html-template:fill-and-print-template 
     *gui-tmpl*
     (list :dispatcher-name (dispatcher-name dispatcher-obj)
           :row-num (length (dispatcher-pool dispatcher-obj)) 
           :rows (loop for item across (dispatcher-pool dispatcher-obj)
                    collect (gen-gui-row dispatcher-obj item))))))

(defun gen-interface-html ()
  "generate interface (homepage) html file from template"
  (with-output-to-string (html-template:*default-template-output*)
    (html-template:fill-and-print-template 
     *interface-tmpl* 
     (list :server-base *server-base*
           :log-path *log-path*
           :rows (loop for key being the hash-keys of *dispatchers*
                      for i from 0
                      collect 
                      (let ((d (gethash key *dispatchers*)))
                        (list :dispatcher-name key :id i 
                              :jobs (dispatcher-count-jobs d)
                              :pending-jobs (dispatcher-count-pending-jobs d))))))))

                                 
                      
                      

     ;; (list :dispatcher-name (dispatcher-name dispatcher-obj)
     ;;       :row-num (length (dispatcher-pool dispatcher-obj)) 
     ;;       :rows (loop for item across (dispatcher-pool dispatcher-obj)
     ;;                collect (gen-gui-row dispatcher-obj item))))))


(defun gen-view-html (file-name)
  "generate view html file for one job"
  (with-output-to-string (html-template:*default-template-output*)
    (html-template:fill-and-print-template 
     *view-tmpl*
     (list :rows (with-open-file (*standard-input* file-name
                                                   :direction :input)
                   (loop for line = (read-line nil nil 'eof)
                        until (eq line 'eof)
                        collect (list :line line)))))))



        
  
  
                                       
  
                               


;; ==================== Handlers ====================
(defmacro when-valid-dispatcher (handler-name (d name) &body body)
  "check whether dispatcher with name 'name' exists. If it does not
exist, signal an error. Otherwise 'body' will be executed. "
  (let ((n (gensym)))
    `(let* ((,n ,name)
            (,d (gethash ,n *dispatchers*)))
       (if (null ,d)
           (progn
             ;; dispatcher not found
             (log-to-file 'error "~a: dispatcher *~a* does not exist." ,handler-name ,n)
             (signal-error))
           (progn
             ,@body)))))

(defmacro when-valid-dispatcher-and-id (handler-name (dispatcher name) id &body body)
  "check whether job with with dispatcher name 'name' and id 'id'
exists. If it does not exist, signal an error. Otherwise 'body' will
be executed. "
  (let ((h (gensym))
        (j (gensym))
        (n (gensym)))
    `(let ((,h ,handler-name)
           (,j ,id)
           (,n ,name))
       (when-valid-dispatcher ,h (,dispatcher ,n) 
         (if (<= (length (dispatcher-pool ,dispatcher)) (parse-integer ,j))
             (progn
               ;; jobid has not been created yet
               (log-to-file 'error "~a: dispatcher *~a* does not spawn job ~a."
                            ,h ,n ,j)
               (signal-error))
             (progn
               ,@body))))))



;; +----------------------------------------
;; | Parameter Setting: server-base
;; | Input: new server-base path
;; | Output: redirect upon successul call and "error" otherwise
;; +----------------------------------------
(hunchentoot:define-easy-handler (update-server-base :uri "/updatebase") (path)
  (setf (hunchentoot:content-type*) "text/plain")
  (setf *server-base* path)
  (ensure-directories-exist (merge-pathnames "imgs/fake" *server-base*))
  (copy-files "../imgs/" (merge-pathnames "imgs/" *server-base*))
  ;; redirect to gui
  (hunchentoot:redirect 
   (format nil "http://~a/gui" (hunchentoot:host))))


;; +----------------------------------------
;; | Parameter Setting: log-path
;; | Input: new log file path
;; | Output: redirect upon successul call and "error" otherwise
;; +----------------------------------------
(hunchentoot:define-easy-handler (update-log-path :uri "/updatelog") (path)
  (setf (hunchentoot:content-type*) "text/plain")
  (setf *log-path* path)
  (ensure-directories-exist *log-path*)
  ;; redirect to gui
  (hunchentoot:redirect 
   (format nil "http://~a/gui" (hunchentoot:host))))

         

;; +----------------------------------------
;; | Create Dispatcher:
;; | Input: Dispatcher's name (name)
;; | Output: "ok" on successul call and "error" otherwise
;; +----------------------------------------
(hunchentoot:define-easy-handler (create-dispatcher :uri "/create") (name)
  (setf (hunchentoot:content-type*) "text/plain")
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
                              :pool (make-vector)
                              :nodes (make-vector)
                              :node-map (make-hash-table :test #'equal)))
       (log-to-file 'done "create: dispatcher ~a created successfully." name)
       (log-to-file 'info "create: dispatcher ~a located at ~a." name 
                    (dispatcher-location (gethash name *dispatchers*)))
       (hunchentoot:redirect 
        (format nil "http://~a/gui" (hunchentoot:host))))))


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
;; | Resubmit job to Dispatcher:
;; | Input: name of dispatcher, job ids
;; | Output: redirect upon successul call and "error" otherwise
;; +----------------------------------------
(hunchentoot:define-easy-handler (resubmit-job :uri "/resubmit") (name jobids)
  (setf (hunchentoot:content-type*) "text/plain")
  (loop for jobid in (split-integers jobids)
     do (when-valid-dispatcher-and-id "resubmit" (d name) (format nil "~a" jobid)
          (let ((j (aref (dispatcher-pool d) jobid)))
            (setf (job-status j) 0))))
  ;; redirect to gui
  (hunchentoot:redirect 
   (format nil "http://~a/gui?name=~a" (hunchentoot:host) name)))


;; +----------------------------------------
;; | Manully Complete (Remove) pending jobs
;; | Input: name of dispatcher, job ids
;; | Output: redirect upon successul call and "error" otherwise
;; +----------------------------------------
(hunchentoot:define-easy-handler (mute-job :uri "/mute") (name jobids)
  (setf (hunchentoot:content-type*) "text/plain")
  (loop for jobid in (split-integers jobids)
     do (when-valid-dispatcher-and-id "mute" (d name) (format nil "~a" jobid)
          (let ((j (aref (dispatcher-pool d) jobid)))
            (setf (job-status j) 2))))
  ;; redirect to gui
  (hunchentoot:redirect 
   (format nil "http://~a/gui?name=~a" (hunchentoot:host) name)))


                                
                                



;; +----------------------------------------
;; | Web Browser GUI
;; | Input: name of dispatcher
;; | Output: the gui webpage
;; +----------------------------------------
(hunchentoot:define-easy-handler (show-gui :uri "/gui") (name)
  (setf (hunchentoot:content-type*) "text/html")
  (if (null name)
      (gen-interface-html)
      (let ((object (gethash name *dispatchers*)))
        (cond (object (log-to-file 'done "gui: gui request completed.")
                      (gen-gui-html object))
              (t (log-to-file 'error "gui: dispatcher *~a* does not exist." name)
                 (signal-error))))))


;; +----------------------------------------
;; | Register slot (machine)
;; | Input: name of dispatcher
;; | Output: shared file list with a leading "ok" upon 
;; |         successul call and "error" otherwise
;; +----------------------------------------
(hunchentoot:define-easy-handler (register :uri "/register") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (when-valid-dispatcher "register" (d name)
    (let* ((local-path (merge-pathnames "shared"
                                        (dispatcher-location d)))
           (file-list (cl-fad:list-directory local-path)))
      ;; register node
      (unless (gethash (hunchentoot:real-remote-addr) (dispatcher-node-map d))
        (push-back (hunchentoot:real-remote-addr) (dispatcher-nodes d))
        (setf (gethash (hunchentoot:real-remote-addr) (dispatcher-node-map d))
              (1- (length (dispatcher-nodes d)))))
      (log-to-file 'done "register: slot registered.")
      (format nil "ok~%~{~a~%~}"
              (loop for file in file-list
                 collect (pathname-fullname file))))))






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
              ;; fetch job successfully
              (let* ((local-path (merge-pathnames (format nil "input/~a/" jobid)
                                                  (dispatcher-location d)))
                     (file-list (cl-fad:list-directory local-path))
                     (node-id (gethash (hunchentoot:real-remote-addr)
                                       (dispatcher-node-map d))))
                (let ((j (aref (dispatcher-pool d) jobid)))
                  ;; update machine information
                  (setf (job-node-id j)
                        node-id)
                  ;; update machine ip
                  (setf (job-ip j)
                        (hunchentoot:real-remote-addr))
                  ;; update start time stamp
                  (setf (job-start-time-stamp j)
                        (get-universal-time)))
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
;; | Output: redirect on successul call and "error" otherwise
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
          (dispatcher-init d)
          (loop for i from 0 
             while (cl-fad:directory-exists-p (merge-pathnames (format nil "input/~a/" i)
                                                               (dispatcher-location d)))
             do
               (push-back (make-job :status 0 :id (length (dispatcher-pool d)))
                          (dispatcher-pool d))
               (log-to-file 'done "reset: dispatcher *~a* picked a job, now have ~a jobs."
                            name  (length (dispatcher-pool d))))
          (hunchentoot:redirect 
           (format nil "http://~a/gui" (hunchentoot:host)))))))


;; +----------------------------------------
;; | Recover dispatcher from a possible last failed run
;; | Input: dispatcher name
;; | Output: redirect on successul call and "error" otherwise
;; +----------------------------------------
(hunchentoot:define-easy-handler (recover-handler :uri "/recover") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((d (gethash name *dispatchers*)))
    (if (null d)
        (progn
          ;; dispatcher not found
          (log-to-file 'error "recover: dispatcher *~a* does not exist." name)
          (signal-error))
        (progn
          (dispatcher-init d)
          (loop for i from 0 
             while (cl-fad:directory-exists-p (merge-pathnames (format nil "input/~a/" i)
                                                               (dispatcher-location d)))
             do
               (push-back (make-job :status 0 :id (length (dispatcher-pool d)))
                          (dispatcher-pool d))
               (log-to-file 'done "recover: dispatcher *~a* picked a job, now have ~a jobs."
                            name (length (dispatcher-pool d))))
          (loop for i below (length (dispatcher-pool d))
             do (when (cl-fad:file-exists-p (merge-pathnames (format nil "output/~a.tar.gz" i)
                                                           (dispatcher-location d)))
                  (progn
                    (dispatcher-set-status d i 'received)
                    (log-to-file 'done "recover: dispatcher *~a* found a completed job." name))))
          (hunchentoot:redirect 
           (format nil "http://~a/gui" (hunchentoot:host)))))))



;; +----------------------------------------
;; | Signal completion of job
;; | Input: dispatcher name, job id
;; | Output: "ok" upon successul call and "error" otherwise
;; +----------------------------------------
(hunchentoot:define-easy-handler (sig-complete :uri "/sigcomplete") (name jobid)
  (setf (hunchentoot:content-type*) "text/plain")
  (when-valid-dispatcher-and-id "sigcomplete" (d name) jobid
    (dispatcher-set-status d (parse-integer jobid) 'complete)
    (log-to-file 'done "sigcomplete: job *~a*:~a complete!" name jobid)
    (signal-ok)))


;; +----------------------------------------
;; | Signal Failure of job
;; | Input: dispatcher name, job id
;; | Output: "ok" upon successul call and "error" otherwise
;; +----------------------------------------
(hunchentoot:define-easy-handler (sig-failure :uri "/sigfailure") (name jobid)
  (setf (hunchentoot:content-type*) "text/plain")
  (when-valid-dispatcher-and-id "sigfailure" (d name) jobid
    (dispatcher-set-status d (parse-integer jobid) 'failure)
    (log-to-file 'done "sigfailure: job *~a*:~a failed ..." name jobid)
    (signal-ok)))



;; +----------------------------------------
;; | Handling upload request
;; | Input: dispatcher name, job id, the multipart form
;; | Output: "ok" on successul call and "error" otherwise
;; +----------------------------------------
(hunchentoot:define-easy-handler (upload-handler :uri "/upload") (name jobid)
  (setf (hunchentoot:content-type*) "text/plain")
  (when-valid-dispatcher-and-id "upload" (d name) jobid
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
                  (signal-error))))))))


;; +----------------------------------------
;; | Handling console output submission
;; | Input: dispatcher name, job id, the multipart form
;; | Output: "ok" on successul call and "error" otherwise
;; +----------------------------------------
(hunchentoot:define-easy-handler (report-handler :uri "/report") (name jobid)
  (setf (hunchentoot:content-type*) "text/plain")
  (when-valid-dispatcher-and-id "report" (d name) jobid
    (let ((post-data (hunchentoot:post-parameter "data")))
      (if (null post-data)
          (progn
            ;; post-data not exist
            (log-to-file 'error "report: *~a*:~a, post-data does not exist."
                         name jobid)
            (signal-error))
          (let ((path (first post-data)))
            (if (copy-file 
                 path 
                 (merge-pathnames (format nil "report/~a/console.output" jobid)
                                  (dispatcher-location d)))
                (progn
                  ;; successful
                  (log-to-file 'done "report: received report for job *~a*:~a!"
                               name jobid)
                  (signal-ok))
                (progn 
                  ;; copy is not successful
                  (log-to-file 'error "report: upload report failed for job *~a*:~a."
                               name jobid)
                  (signal-error))))))))


;; +----------------------------------------
;; | Handler for viewing console report
;; | Input: dispatcher name, job id
;; | Output: the console report
;; +----------------------------------------
(hunchentoot:define-easy-handler (view-handler :uri "/view") (name jobid)
  (setf (hunchentoot:content-type*) "text/html")
  (when-valid-dispatcher-and-id "view" (d name) jobid
    (let ((path (merge-pathnames (format nil "report/~a/console.output" jobid)
                                 (dispatcher-location d))))
      (if (cl-fad:file-exists-p path)
          (progn
            (log-to-file 'done "view: console report for *~a*:~a trasmitted."
                         name jobid)
            (gen-view-html path))
          (progn
            (log-to-file 'done "view: job concole report for *~a*:~a hasn't been established yet."
                         name jobid)
            "no report available.")))))
                    
                     

(hunchentoot:define-easy-handler (test-handler :uri "/test") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~a~%" (hunchentoot:real-remote-addr)))
      


;; ==================== server-side controllers ====================

(defun start-server (&optional (port 4242))
  "Start the server with/without a specific port"
  ;; start server
  (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor :port port
                                  :document-root *server-base*))
  (hunchentoot:start *acceptor*)
  (format t "server started.~%"))

(defun stop-server ()
  "Stop the server"
  (setf *dispatchers* (make-hash-table :test #'equal))
  (hunchentoot:stop *acceptor*)
  (format t "server stopped.~%"))





