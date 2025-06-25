(in-package :slashcord/types)

;; https://discord.com/developers/docs/interactions/receiving-and-responding#interaction-object-application-command-interaction-data-option-structure
(defclass resolved (json-encodable)
  ((users
    :initarg :users
    :json-key "users")
   (members
    :initarg :members
    :json-key "members")
   (roles
    :initarg :roles
    :json-key "roles")
   (channels
    :initarg :channels
    :json-key "channels")
   (messages
    :initarg :messages
    :json-key "messages")
   (attachments
    :initarg :attachments
    :json-key "attachments"))

  (:metaclass json-serializable-class))
