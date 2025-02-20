(defpackage slashcord
  (:use :cl)
  (:local-nicknames (:a alexandria)
                    (:i :ironclad)
                    (:f flexi-streams)
                    (:s serapeum)
                    (:h :hunchentoot))
  (:import-from :serapeum :-> :dict)
  (:import-from :easy-routes :defroute)
  (:import-from :json-mop :json-serializable-class)
  (:export :main))
(in-package :slashcord)

(defvar *server* nil)
(setf h:*show-lisp-errors-p* t)
(defvar *public-key* (uiop:getenv "SLASHCORD_PUBLIC_KEY"))

(assert
 (and *public-key*
      (ignore-errors (i:hex-string-to-byte-array *public-key*)))
        (*public-key*)
        "Please set SLASHCORD_PUBLIC_KEY to your application public key.")

(defvar +signature-header+ :x-signature-ed25519)
(defvar +timestamp-header+ :x-signature-timestamp)
(defvar +slashcord-default-port+ (or
                                  (ignore-errors (parse-integer (uiop:getenv "SLASHCORD_PORT")))
                                  4242))

;; https://discord.com/developers/docs/interactions/receiving-and-responding#security-and-authorization
(-> valid-signature-p (string string string string) boolean)
(defun valid-signature-p (public-key body signature timestamp)
  "Verify an incoming Discord interaction against the application public key"
  (handler-case
    (a:if-let ((verify-key (i:make-public-key :ed25519 :y (i:hex-string-to-byte-array public-key)))
               (signature-bytes (i:hex-string-to-byte-array signature))
               (body-bytes (i:ascii-string-to-byte-array (concatenate 'string timestamp body))))
      (i:verify-signature
       verify-key
       body-bytes
       signature-bytes))
    (i:ironclad-error (c)
      "TODO: logging or better handling here"
      (values nil c))))

(-> json-dict (&rest t) string)
(defun json-dict (&rest vars)
  (yason:encode-object (apply #'dict vars)))

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
              (yason:parse (h:raw-post-data :force-text t)))))
    (yason:encode slashcord/types:ping)))

(defun start (&key (port +slashcord-default-port+))
  (setf *server* (make-instance 'easy-routes:routes-acceptor :port port))
  (h:start *server*))

(defun stop ()
  (h:stop *server*))

(defun main ()
  (start :port +slashcord-default-port+)
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
