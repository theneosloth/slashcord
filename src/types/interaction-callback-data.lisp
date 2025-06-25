(in-package :slashcord/types)

;; TODO: Similar type checking for other flags
;; https://discord.com/developers/docs/interactions/receiving-and-responding#interaction-response-object-interaction-callback-type
(defvar +pong-callback+ 1)
(defvar +channel-message-with-source-callback+ 4)

(deftype interaction-callback-type ()
  '(or
    (integer 4 10)
    (member 1 12)))

(deftype message-flags ()
  `(integer))

(defclass interaction-callback (json-encodable)
  ((tts :initarg :tts :type boolean :json-type :bool :json-key "tts")
   (content :initarg :content :type string :json-key "content")
   (embeds :initarg :embeds :type list :json-key "embeds")
   (allowed-mentions :initarg :allowed-mentions :type list :json-key "allowed_mentions")
   (flags :initarg :flags :type message-flags :json-key "flags")
   (components :initarg :components :type t :json-key "components")
   (attachments :initarg :attachments :type t :json-key "attachments")
   (poll :initarg :poll :type t :json-key "poll"))
  (:metaclass json-serializable-class))
