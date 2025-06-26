(defpackage slashcord/rest
  (:use :cl)
  (:local-nicknames (:d dexador) (:s serapeum))
  (:import-from :slashcord/types :to-json :from-json :application-command-post)
  (:export
   #:list-commands
   #:make-client
   #:get-bot-token
   #:get-application-id
   #:create-command
   #:delete-command
   #:create-guild-command
   #:command-uri
   #:commands-uri))
(in-package :slashcord/rest)

(defclass discord-api-client ()
  ((headers :initarg :headers :accessor headers :type list :initform nil)
   (application-id :initarg :application-id :type string :accessor application-id
                   :initform (error "application-id not provided"))))

(defun make-client (application-id token)
  (let* ((token-header (format nil "Bot ~a" token))
         (headers `(("Content-Type" . "application/json")
                    ("User-Agent" . "SlashCord (https://github.com/theneosloth/slashcord, 0.0.1)")
                    ("Authorization" . ,token-header))))
    (make-instance 'discord-api-client :application-id application-id :headers headers)))

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
  (:method ((client discord-api-client))
    (format nil "https://discord.com/api/v10/applications/~a/commands" (application-id client))))

(defgeneric command-uri (discord-api-client string)
  (:method ((client discord-api-client) (id string))
    (format nil "https://discord.com/api/v10/applications/~a/commands/~a" (application-id client) id)))

(defmethod command-uri((client discord-api-client) (command slashcord/types::application-command))
  (format nil "https://discord.com/api/v10/applications/~a/commands/~a" (application-id client) (slot-value command 'slashcord/types::id)))

(defgeneric guild-commands-uri (discord-api-client id)
  (:method ((client discord-api-client) guild-id)
      (format nil "https://discord.com/api/v10/applications/~a/guilds/~a/commands" (application-id client) guild-id)))

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
    (slashcord/types:from-json res class)))

(defmethod api-delete ((client discord-api-client) uri)
  (let* ((res (request client uri :method :DELETE)))
    res))

(defmethod api-post ((client discord-api-client) uri obj class)
  (let* ((body (slashcord/types:to-json obj))
         (res (request client uri :method :POST :body body))
         (res (yason:parse res
                           :json-booleans-as-symbols t
                           :json-arrays-as-vectors t
                           :json-nulls-as-keyword t)))
    (slashcord/types:from-json res class)))

(defmethod create-command ((client discord-api-client) (command slashcord/types::application-command-post))
  (api-post client (commands-uri client) command 'slashcord/types::application-command))

(defmethod create-guild-command ((client discord-api-client) (command slashcord/types::application-command-post) guild-id)
  (api-post client (guild-commands-uri client guild-id) command 'slashcord/types::application-command))

(defmethod list-commands ((client discord-api-client))
  (let* ((res (api-get client (commands-uri client) 'slashcord/types::application-command)))
    res))

(defgeneric delete-command (discord-api-client slashcord/types::application-command)
  (:method ((client discord-api-client) (command slashcord/types::application-command))
    (let ((url (command-uri client command)))
      (api-delete client url))))

(defmethod delete-command ((client discord-api-client) (id string))
  (api-delete client (command-uri client id)))
