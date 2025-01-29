(in-package :slashcord)

(defvar *server* nil)
(setf h:*show-lisp-errors-p* t)
(defvar *public-key* (uiop:getenv "PUBLIC_KEY"))

(defvar +signature-header+ :X-SIGNATURE-ED25519)
(defvar +timestamp-header+ :X-SIGNATURE-TIMESTAMP)
(defvar +slashcord-default-port+ 4242)


;; https://discord.com/developers/docs/interactions/receiving-and-responding#security-and-authorization

(-> valid-signature-p (string string string string) boolean)
(defun valid-signature-p (public-key body signature timestamp)
  (handler-case
    (a:if-let ((verify-key (i:make-public-key :ed25519 :y (i:hex-string-to-byte-array public-key)))
               (signature-bytes (i:hex-string-to-byte-array signature))
               (body-bytes (i:ascii-string-to-byte-array (concatenate 'string timestamp body))))
      (i:verify-signature
       verify-key
       body-bytes
       signature-bytes))
    (i:ironclad-error (c)
      (format t "We handled an error: ~a~%" c)
      (values nil c))))

(defun json-dict (&rest vars)
  (jzon:stringify (apply #'dict vars)))

(defun @json (next)
  (setf (hunchentoot:content-type*) "application/json")
  (funcall next))

(defun @auth (next)
  (a:if-let ((body (h:raw-post-data :force-text t))
             (signature (cdr (assoc +signature-header+ (h:headers-in*))))
             (timestamp (cdr (assoc +timestamp-header+ (h:headers-in*)))))
    (if (valid-signature-p *public-key* body signature timestamp)
        (funcall next)
        (easy-routes:http-error 401))
    (easy-routes:http-error h:+http-bad-request+)))

(defroute pong-get ("/" :method :get :decorators (@json)) ()
  (easy-routes:http-error h:+http-bad-request+))

(defroute pong-post ("/" :method :post :decorators (@auth @json)) ()
  (a:if-let
      ((json (ignore-errors
              (jzon:parse (h:raw-post-data :force-text t)))))
    (jzon:stringify ping)))

(defun start (&key (port 4242))
  (setf *server* (make-instance 'easy-routes:routes-acceptor :port port))
  (h:start *server*))

(defun stop ()
  (h:stop *server*))

(defun main ()
  (start :port (or (uiop:getenv "SLASHCORD_PORT") +SLASHCORD-DEFAULT-PORT+))
  (handler-case (bt:join-thread (find-if (lambda (th)
                                            (search "hunchentoot" (bt:thread-name th)))
                                         (bt:all-threads)))
    ;; Catch a user's C-c
    (#+sbcl sb-sys:interactive-interrupt
      #+ccl  ccl:interrupt-signal-condition
      #+clisp system::simple-interrupt-condition
      #+ecl ext:interactive-interrupt
      #+allegro excl:interrupt-signal
      () (progn
           (format *error-output* "Aborting.~&")
           (h:stop *server*)
           (uiop:quit)))
    (error (c) (format t "Woops, an unknown error occured:~&~a~&" c))))
