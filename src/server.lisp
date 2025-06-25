(defpackage slashcord/server
  (:use :cl :slashcord/types)
  (:local-nicknames (:a alexandria)
                    (:i :ironclad)
                    (:f flexi-streams)
                    (:s serapeum)
                    (:h :hunchentoot))
  (:import-from :alexandria :if-let)
  (:import-from :serapeum :-> :dict :etypecase-of)
  (:import-from :easy-routes :defroute)
  (:export :main :start :stop :make-event-handler))
(in-package :slashcord/server)

(defparameter *server* nil)
(defparameter *events* (make-hash-table :test #'equal))

(setf h:*show-lisp-errors-p* t)

(defvar +signature-header+ :x-signature-ed25519)
(defvar +timestamp-header+ :x-signature-timestamp)
(defvar +slashcord-port+ (or
                                  (ignore-errors (parse-integer (uiop:getenv "SLASHCORD_PORT")))
                                  4242))

(define-condition slashcord-error (error) ()
  (:documentation "Superclass for slashcord conditions"))

(define-condition unknown-interaction (slashcord-error)
  ((%interaction-id :initarg :interaction-id :type integer :reader interaction-id))
  (:documentation "Condition raised when the server receives an unknown interaction.")
  (:report (lambda (c s)
             (format s "Unknown interaction received: ~a" (interaction-id c)))))

;; https://discord.com/developers/docs/interactions/receiving-and-responding#security-and-authorization
(-> valid-signature-p (string string string string) boolean)
(defun valid-signature-p (public-key body signature timestamp)
  "Verify an incoming Discord interaction against the application public key"
  (handler-case
      (if-let ((verify-key (i:make-public-key :ed25519 :y (i:hex-string-to-byte-array public-key)))
                 (signature-bytes (i:hex-string-to-byte-array signature))
                 (body-bytes (i:ascii-string-to-byte-array (concatenate 'string timestamp body))))
        (i:verify-signature
         verify-key
         body-bytes
         signature-bytes))
    (i:ironclad-error (c)
      "TODO: logging or better handling here"
      (values nil c))))

(defun @json (next)
  "Hunchentoot decorator that sets the content type to json."
  (setf (hunchentoot:content-type*) "application/json")
  (funcall next))

(defun @auth (next)
  "Hunchentoot decorate that validates received Discord interactions against the application public key."
  (if-let ((body (h:raw-post-data :force-text t))
             (signature (cdr (assoc +signature-header+ (h:headers-in*))))
             (timestamp (cdr (assoc +timestamp-header+ (h:headers-in*)))))
    (if (valid-signature-p (get-public-key) body signature timestamp)
        (funcall next)
        (easy-routes:http-error 401))
    (easy-routes:http-error h:+http-bad-request+)))

(-> get-event-handler (slashcord/types::interaction-type) t)
(defun get-event-handler (interaction-id)
  (gethash interaction-id *events*))

(-> make-event-handler (t function) nil)
(defun make-event-handler (event-name event-handler)
  (setf (gethash event-name *events*) event-handler))

(defun handle-ping (interaction)
  (declare (ignorable interaction))
  (let ((pong (to-json slashcord/types::pong)))
    pong))

(defun handle-interaction (interaction)
  (let* ((interaction-type (slot-value interaction 'slashcord/types::type))
         (handler (get-event-handler interaction-type)))
    (etypecase-of t handler
      (function (funcall handler interaction))
      (t (error "No handler configured for event")))))

(defroute receive-interaction ("/" :method :post :decorators (@auth @json)) ()
  "Route that receives json interactions and delegates them to interaction-handlers"
  (handler-case
      (let*
          ((json (yason:parse
                  (h:raw-post-data :force-text t)
                  :json-booleans-as-symbols t
                  :json-arrays-as-vectors t
                  :json-nulls-as-keyword t))
           (interaction-type (gethash "type" json)))
        (etypecase-of t interaction-type
          (slashcord/types::interaction-type (handle-interaction (from-json json 'slashcord/types::interaction)))
          (t (error 'unknown-interaction :interaction-id interaction-type))))
    (error (c)
      (format *error-output* "Interaction handler encountered an error: ~a.~&" c)
      (easy-routes:http-error h:+http-bad-request+))))

(-> get-public-key () string)
(defun get-public-key ()
  (or
   (uiop:getenv "SLASHCORD_PUBLIC_KEY")
   (error "Could not find public key")))

(defun start (&key (port +slashcord-port+))
  "Start a background thread running Slashcord on a given port"
  (make-event-handler slashcord/types::+interaction-ping+ #'handle-ping)
  (setf *server* (make-instance 'easy-routes:routes-acceptor :port port))
  (h:start *server*))

(defun stop ()
  (h:stop *server*))

(defun main ()
  "Blocking function with ctrl+c handling"
  (assert
   (and (get-public-key)
        (ignore-errors (i:hex-string-to-byte-array (get-public-key))))
   ()
   "Please set SLASHCORD_PUBLIC_KEY to your application public key.")
  (format *error-output* "Running slashcord on ~D~&" +slashcord-port+)
  (finish-output)
  (start :port +slashcord-port+)
  (handler-case (bt:join-thread (find-if (lambda (th)
                                           (search "hunchentoot" (bt:thread-name th)))
                                         (bt:all-threads)))
    ;; Catch a user's C-c
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+allegro excl:interrupt-signal
     () (progn
          (format *error-output* "Aborting.~&")
          (h:stop *server*)
          (uiop:quit)))
    (error (c) (format t "Woops, an unknown error occured:~&~a~&" c))))

(defun cleanup ()
  (bt:destroy-thread
   (find-if (lambda (th)
              (search "hunchentoot" (bt:thread-name th)))
            (bt:all-threads))))
