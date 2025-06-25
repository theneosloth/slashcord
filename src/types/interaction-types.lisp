(in-package :slashcord/types)

;; https://discord.com/developers/docs/interactions/receiving-and-responding#interaction-object-interaction-type
(defparameter +interaction-ping+ 1)
(defparameter +interaction-application-command+ 2)
(defparameter +interaction-message-component+ 3)
(defparameter +interaction-application-command-autocomplete+ 4)
(defparameter +interaction-modal-submit+ 5)

(defparameter +interaction-types+ (list +interaction-ping+
                                    +interaction-application-command+
                                    +interaction-message-component+
                                    +interaction-application-command-autocomplete+
                                    +interaction-modal-submit+))

(deftype interaction-type ()
  `(member thing ,@+interaction-types+))

;; TODO: check
;; ^[-_\p{L}\p{N}\p{sc=Deva}\p{sc=Thai}]{1,32}$
(deftype interaction-name ()
  '(string))
