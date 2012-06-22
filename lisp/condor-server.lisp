;; Data structure
(defstruct dispatcher name id)

;; The Global Acceptor
(defvar *acceptor* nil "the condor dispatchers acceptor")
(defparameter *dispatcher-count* 0)



(hunchentoot:define-easy-handler (create-dispatcher :uri "/create") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((id *dispatcher-count*))
    (if (eq (hunchentoot:request-method*) :GET)
        (format nil "Sorry, buddy, but /create do not offer webpage mode.")
        (progn
          (incf *dispatcher-count*)
          (format nil "~a:~a~%" id name)))))


(defun start-server (&optional (port 8855))
  "Start the server with/without a specific port"
  (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor :port port))
  ;; ==================== Handlers ====================
  ;; +----------------------------------------
  ;; | Create Dispatcher:
  ;; | Input: Dispatcher's name (name)
  ;; | Output: Dispatcher's Identifier (id)
  ;; +----------------------------------------
  (hunchentoot:start *acceptor*))

(defun stop-server ()
  "Stop the server"
  (hunchentoot:stop *acceptor*))




