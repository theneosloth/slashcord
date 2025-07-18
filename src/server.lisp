(defpackage slashcord/server
  (:use :cl)
  (:local-nicknames (:a alexandria)
                    (:i :ironclad)
                    (:f flexi-streams)
                    (:h :hunchentoot))
  (:import-from :slashcord/types :from-json :to-json)
  (:import-from :alexandria :if-let)
  (:import-from :serapeum :-> :etypecase-of)
  (:import-from :easy-routes :defroute)
  (:export :main :start :stop :define-handler))
(in-package :slashcord/server)

(defparameter *server* nil)
(defparameter *event-handlers* (make-hash-table :test #'equal))

(defvar +signature-header+ :x-signature-ed25519)
(defvar +timestamp-header+ :x-signature-timestamp)
(defvar +slashcord-port+ (or
                          (ignore-errors (parse-integer (uiop:getenv "SLASHCORD_PORT")))
                          4242))
(defvar +public-key+ nil)

(-> valid-signature-p (string string string string) boolean)
(defun valid-signature-p (public-key body signature timestamp)
  "Verify an incoming Discord interaction against the application public key
   See: https://discord.com/developers/docs/interactions/receiving-and-responding#security-and-authorization"
  (handler-case
      (if-let ((verify-key (i:make-public-key :ed25519 :y (i:hex-string-to-byte-array public-key)))
                 (signature-bytes (i:hex-string-to-byte-array signature))
                 (body-bytes (i:ascii-string-to-byte-array (concatenate 'string timestamp body))))
        (i:verify-signature
         verify-key
         body-bytes
         signature-bytes))
    (i:ironclad-error (c)
      "TODO: logging or better handling of error message"
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
    (if (valid-signature-p +public-key+ body signature timestamp)
        (funcall next)
        (easy-routes:http-error 401))
    (easy-routes:http-error h:+http-bad-request+)))

(-> get-event-handler (slashcord/types:interaction-type) t)
(defun get-event-handler (event)
  (gethash event *event-handlers*))

(-> bind-event-handler (t function) nil)
(defun bind-event-handler (event event-handler)
  (setf (gethash event *event-handlers*) event-handler))

(defmacro define-handler (interaction-name (arg) &body body)
  "Create a handler for INTERACTION-NAME. The name is converted into a numeric id and is stored in an event-handler hash.
  The BODY is expected to be a function that receives an interaction object ARG and returns a slashcord/types:interaction-callback
  The available handlers are defined in types/util.lisp

  Example:
  (define-handler :ping (interaction)
  slashcord/types::pong)"

  (let ((interaction-gensym (gensym))
        (function-gensym (gensym)))
    `(let ((,interaction-gensym (slashcord/types:get-interaction-id ,interaction-name)))
       (flet ((,function-gensym (,arg) ,@body))
         (setf (gethash ,interaction-gensym *event-handlers*)
               #',function-gensym)))))

(define-handler :ping (interaction)
  "Return the expected PONG object for Discord API ping requests. Ignores the value of INTERACTION."
  (declare (ignorable interaction))
  slashcord/types::pong)

(defun handle-interaction (interaction)
  "Helper function that dispatches an INTERACTION object to an appropriate event handler."
  (let* ((interaction-type (slot-value interaction 'slashcord/types:type))
         (handler (get-event-handler interaction-type)))
    (etypecase handler
      (function (to-json (funcall handler interaction)))
      (t (error "No handler configured for event ~a.~&" interaction-type)))))

(defroute receive-interaction ("/" :method :post :decorators (@auth @json)) ()
  "Route that receives json interactions and delegates them to interaction handlers"
  (let*
      ((json (yason:parse
              (h:raw-post-data :force-text t)
              :json-booleans-as-symbols t
              :json-arrays-as-vectors t
              :json-nulls-as-keyword t))
       (interaction-type (gethash "type" json)))
    (etypecase-of (or number null slashcord/types:interaction-type) interaction-type
      (slashcord/types::interaction-type (handle-interaction (from-json json 'slashcord/types:interaction)))
      (number (error "Received data does not contain a supported interaction, id received: ~a.~&" interaction-type))
      (null (error "Received data does not contain an interaction-type.~&")))))

(defroute liveness ("/healthz" :method :get :decorators ()) ()
  "Simple liveness check endpoint."
  "ok")

(-> get-public-key () string)
(defun get-public-key ()
  "Retrieve the key from SLASHCORD_PUBLIC_KEY"
  (or
   (uiop:getenv "SLASHCORD_PUBLIC_KEY")
   (error "Could not find public key")))

(defun start (&key (port +slashcord-port+) (public-key (get-public-key)))
  "Start a background thread running Slashcord on PORT.
   Uses PUBLIC-KEY or the environment variable SLASHCORD_PUBLIC_KEY"
  (setf +public-key+ public-key)
  (assert
   (and +public-key+
        (ignore-errors (i:hex-string-to-byte-array (get-public-key))))
   ()
   "Public key not set.")
  (setf *server* (make-instance 'easy-routes:routes-acceptor :port port))
  (h:start *server*)
  (format *error-output* "Running slashcord on ~D~&" +slashcord-port+)
  (finish-output))

(defun stop ()
  (h:stop *server*))

(defun main ()
  "Blocking function with ctrl+c handling"
  (start)
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
