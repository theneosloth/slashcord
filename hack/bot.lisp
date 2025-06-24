(defpackage lfg-bot
  (:use :cl)
  (:local-nicknames (:c :slashcord/rest) (:s :slashcord/server))
  (:import-from :slashcord-types :to-json +channel-message-with-source-callback+ interaction-response interaction-callback))

(in-package :lfg-bot)

(defvar *lfg-table* (make-hash-table :test #'equal))
(defvar *lfg-command-options* (list
                               (make-instance 'slashcord-types::command-option
                                              :type slashcord-types:+option-integer+
                                              :name "playercount"
                                              :description "How many players are you looking for?"
                                              :required t
                                              :min-value 1)))

(defvar *lfg-command* (make-instance 'slashcord-types::application-command-post
                                     :name "lfg"
                                     :description "Initialize an LFG queue"
                                     :options *lfg-command-options*
                                     :type 1))

(defvar *join-command* (make-instance 'slashcord-types::application-command-post
                                     :name "join"
                                     :description "Add yourself to the active LFG queue"
                                     :options #()
                                     :type 1))

(defun make-text-response (msg)
  (make-instance
   'interaction-response
   :type +channel-message-with-source-callback+
   :data (make-instance 'interaction-callback :content (format nil "~a" msg))))

(defun lfg-handler ()
  (lambda (interaction)
    (let* ((interaction-channel (slot-value  (slot-value interaction 'slashcord-types::channel) 'slashcord-types::id))
           (example (make-text-response (format nil "~a" (inc-key *lfg-table* interaction-channel)))))
      example)))

(defun create-lfg-commands ()
  (let ((client (c:make-client (c:get-application-id) (c:get-bot-token))))
    (s::make-command-handler "lfg" (lfg-handler))
    (s::make-command-handler "join" (lfg-handler))
    (c:create-command client *lfg-command*)
    (c:create-command client *join-command*)))

(defmethod inc-key ((table hash-table) key)
  (if (gethash key table)
      (incf (gethash key table))
      (setf (gethash key table) 1)))
