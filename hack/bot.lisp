(defpackage lfg-bot
  (:use :cl)
  (:local-nicknames (:c :slashcord/rest) (:s :slashcord/server))
  (:import-from :slashcord/types :make-option)
  (:import-from :serapeum :->))

(in-package :lfg-bot)

(defvar *lfg-table* (make-hash-table :test #'equal))
(defvar *lfg-command*  (slashcord/types:make-command "lfg" "Initialize an LFG queue." :options (vector (make-option "playercount" "How many players are you looking for?" :integer :required t))))

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

(defmethod inc-key ((table hash-table) key)
  (if (gethash key table)
      (incf (gethash key table))
      (setf (gethash key table) 1)))

(slashcord/server:define-handler :COMMAND (interaction)
  (let* ((interaction-channel (slot-value  (slot-value interaction 'slashcord/types::channel) 'slashcord/types::id))
         (example (slashcord/types:make-text-callback (format nil "~a" (inc-key *lfg-table* interaction-channel)))))
    example))

(defun create-lfg-commands ()
  (let ((client (c:make-client (get-application-id) (get-bot-token))))
    (c:create-command client *lfg-command*)))

(defun list-commands ()
  (let* ((client (c:make-client (get-application-id) (get-bot-token)))
         (commands (c:list-commands client)))
    commands))

(defun main ()
  (create-lfg-commands)
  (list-commands)
  (s:start))
