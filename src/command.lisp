(defpackage slashcord/commands
  (:use :cl)
  (:import-from :slashcord/server :make-command-handler)
  (:import-from :slashcord/types :make-option :make-command)
  (:import-from :slashcord/rest :create-command :make-client)
  (:import-from :serapeum :->))
(in-package :slashcord/commands)

(defparameter *commands* (make-hash-table :test #'equal))

(-> get-bot-token () string)
(defun get-bot-token ()
  (or
   (uiop:getenv "SLASHCORD_BOT_TOKEN")
   (error "Could not find SLASHCORD_BOT_TOKEN")))

(-> get-application-id () string)
(defun get-application-id ()
  (or
   (uiop:getenv "SLASHCORD_APPLICATION_ID")
   (error "Could not find SLASHCORD_APPLICATION_ID")))

(-> create-command (slashcord/types::application-command-post t) t)
(defun create-command (command callback)
  (let ((client (make-client (get-application-id) (get-bot-token)))
         (command-name (slot-value command 'slashcord/types::name)))
    (slashcord/rest:create-command client command)
    (slashcord/server:make-command-handler command-name callback)))

(-> make-text-response (slashcord/types::interaction) slashcord/types::interaction-response)
(defun make-text-response (msg)
  (make-instance
   'slashcord/types::interaction-response
   :type 'slashcord/types::+channel-message-with-source-callback+
   :data (make-instance 'slashcord/types::interaction-callback :content (format nil "~a" msg))))
