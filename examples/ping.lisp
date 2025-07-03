(defpackage ping-bot
  (:use :cl)
  (:import-from :slashcord/types :make-command :make-option)
  (:import-from :slashcord/rest :make-client :create-command)
  (:import-from :slashcord/server :start :define-handler))

(in-package :ping-bot)

(defvar +bot-token+ (uiop:getenv "SLASHCORD_BOT_TOKEN"))
(defvar +bot-id+ (uiop:getenv "SLASHCORD_APPLICATION_ID"))
(defvar +server-public-key+ (uiop:getenv "SLASHCORD_PUBLIC_KEY"))

(slashcord/server:define-handler :command (interaction)
  "Return a Pong! message.
   Note that this is a handler for all commands received, per-command dispatching would have to be implemented manually.
   All handlers receive a slashcord/types:interaction and are expected to return a slashcord/types:interaction-callback"
  (declare (ignorable interaction))
  (let* ((pong-response (slashcord/types:make-text-callback "Pong!")))
    pong-response))

(defun create-ping-command ()
  "Register a ping command on the discord API."
  (let ((client (make-client +bot-id+ +bot-token+))
        (ping-command (make-command "ping" "Send a Pong!")))
    (create-command client ping-command)))

(defun main ()
  (create-ping-command)
  (start))
