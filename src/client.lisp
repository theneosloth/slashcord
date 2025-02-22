(defpackage slashcord-client
  (:use :cl :slashcord-types)
  (:local-nicknames (:d dexador) (:s serapeum))
  (:export
   #:list-commands))
(in-package :slashcord-client)

(defvar *bot-token* (uiop:getenv "SLASHCORD_BOT_TOKEN"))
(defvar *application-id* (uiop:getenv "SLASHCORD_APPLICATION_ID"))

(defvar *token-header* (format nil "Bot ~a" *bot-token*))

(defclass discord-api-client ()
  ((headers :initarg :headers :accessor headers :type list :initform `(("Content-Type" . "application/json")
                                                                       ("User-Agent" . "DiscordBot (example.com, 0.0.1)")
                                                                       ("Authorization" . ,*token-header*)))
   (application-id :initarg :application-id :type string :accessor application-id
                   :initform (error "application-id not provided"))))


(defun make-client (application-id token)
  (let* ((token-header (format nil "Bot ~a" token))
         (headers `(("Content-Type" . "application/json")
                    ("User-Agent" . "DiscordBot (example.com, 0.0.1)")
                    ("Authorization" . ,token-header))))
    (make-instance 'discord-api-client :application-id application-id :headers headers)))

(defparameter *client* (make-client *application-id* *bot-token*))

(define-condition discord-error (error)
  ((status :initarg :status :initform nil)
   (code :initarg :code :initform nil)
   (message :initarg :message :initform nil))
  (:documentation "A wrapper around discord REST API errors")
  (:report (lambda (condition stream)
             (with-slots (status code message) condition
               (format stream "Discord API returned ~D with code ~D: ~a" status code message)))))

(define-condition decoding-error (error)
  ()
  (:documentation "The error raised when non JSON data is received"))

(defgeneric commands-uri (discord-api-client)
  (:method (client)
      (format nil "https://discord.com/api/v10/applications/~a/commands" (application-id client))))


(defmethod request ((client discord-api-client) uri &key (method :get) body)
  (with-slots (headers) client
    (handler-case
        (dexador:request
         uri
         :method method
         :headers headers
         :content body
         :keep-alive nil)
      (dex:http-request-failed (e)
        (let* ((status (dex:response-status e))
               (body (dex:response-body e))
               (response (ignore-errors (yason:parse body))))
          (case status
            (404 (error 'discord-error :status 404 :message "404 not found"))
            (t
             (error 'discord-error :status status :code (gethash "code" response) :message (gethash "message" body)))))))))

(defmethod api-get ((client discord-api-client) uri class)
  (let* ((res (request client uri))
         (res (yason:parse res
                           :json-booleans-as-symbols t
                           :json-arrays-as-vectors t
                           :json-nulls-as-keyword t)))
    (from-json res class)))

(defmethod api-post ((client discord-api-client) uri obj class)
  (let* ((body (to-json obj))
         (res (request client uri :method :POST :body body))
         (res (yason:parse res
                           :json-booleans-as-symbols t
                           :json-arrays-as-vectors t
                           :json-nulls-as-keyword t)))
    (from-json res class)))

(defmethod create-command ((client discord-api-client) (command application-command-post))
  (api-post client (commands-uri client) command 'application-command))

(defmethod list-commands ((client discord-api-client))
  (let* ((res (api-get client (commands-uri client) 'application-command)))
    res))

(defun create-ping-command ()
  (let* ((text-option (make-instance 'command-option
                                     :type +option-string+
                                     :name "input"
                                     :description "The command input"
                                     :required t))
         (command (make-instance 'application-command-post
                                 :name "slash"
                                 :type +command-chat-input+
                                 :description "Slashcord demo"
                                 :options (list text-option))))
    (create-command *client* command)))
