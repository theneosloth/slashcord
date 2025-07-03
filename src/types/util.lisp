(in-package :slashcord/types)

(-> get-interaction-id ((member :ping :command :autocomplete :modal)) integer)
(defun get-interaction-id (interaction)
  (ecase interaction
    (:ping +interaction-ping+)
    (:command +interaction-application-command+)
    (:autocomplete +interaction-application-command-autocomplete+)
    (:modal +interaction-modal-submit+)))

(-> make-option (string string (member :string :integer :boolean) &key (:required boolean) (:choices vector) (:options vector)) t)
(defun make-option (name description type &key required choices options)
  "Make a command option of a given TYPE. Defaults to string"
  (let* ((types (list :string +option-string+
                      :integer +option-integer+
                      :boolean +option-boolean+))
         (type-val (getf types type +option-string+)))
    (make-instance 'command-option
                   :type type-val
                   :name name
                   :description description
                   :required required
                   :choices (coerce choices 'vector)
                   :options (coerce choices 'vector))))

(-> make-command (string string &key (:options vector)) t)
(defun make-command (name description &key options)
  (make-instance 'application-command-post
                 :name name
                 :description description
                 :type 1
                 :options (or options #())))

(-> make-text-response (string) interaction-callback)
(defun make-text-callback (msg)
  (make-instance
   'interaction-callback
   :type +channel-message-with-source-callback+
   :data (make-instance 'interaction-callback-data :content (format nil "~a" msg))))
