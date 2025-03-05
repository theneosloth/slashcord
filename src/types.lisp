(defpackage slashcord-types
  (:use :cl)
  (:import-from :serapeum :-> :dict)
  (:import-from :easy-routes :defroute)
  (:import-from :json-mop :json-serializable-class)
  (:export
   :encode
   :ping
   :command-choice
   :application-command-post
   :command-option
   :interaction-response
   :application-command
   :to-json
   :from-json
   :json-encodable
   :+option-string+
   :+option-boolean+
   :+command-chat-input+
   :interaction-callback
   :+interaction-ping+
   :+interaction-application-command+
   :+interaction-message-component+
   :+interaction-application-command-autocomplete+
   :+interaction-modal-submit+))

(in-package :slashcord-types)

;; Maybe there's an OpenAPI definition we can use to generate the definitions?

(deftype snowflake () `(integer 0 (,most-positive-fixnum)))

(-> snowflake (string) snowflake)
(defun snowflake (timestamp)
  (coerce (parse-integer timestamp) 'snowflake))

;; TODO: check
;; ^[-_\p{L}\p{N}\p{sc=Deva}\p{sc=Thai}]{1,32}$
(deftype name ()
  '(string))

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

(-> interaction-p (t) boolean)
(defun interaction-p (thing)
  (member thing +interaction-types+))

(deftype interaction-type ()
  `(satisfies interaction-p))

;; TODO: Similar type checking for other flags
;; https://discord.com/developers/docs/interactions/receiving-and-responding#interaction-response-object-interaction-callback-type
(defparameter +ping-callback+ 1)
(deftype interaction-callback-type ()
  '(or
    (integer 4 10)
    (member 1 12)))

(deftype message-flags ()
  `(integer))

(defclass json-encodable ()
  ()
  (:documentation "A class that can be directly encoded into json"))

(defmethod to-json ((encodable json-encodable))
  (with-output-to-string (json)
    (json-mop:encode encodable json)))

(defmethod from-json ((input sequence) class &rest initargs)
  (declare (ignore initargs))
  (etypecase input
    (list
     (mapcar (lambda (element)
               (json-mop:json-to-clos element class))
             input))
    (vector
     (map 'vector
          (lambda (element)
            (json-mop:json-to-clos element class))
          input))))

(defmethod from-json ((input hash-table) class &rest initargs)
  (declare (ignore initargs))
  (json-mop:json-to-clos input class))


(defclass interaction-data (json-encodable)
  ((id :initarg :id :type snowflake :initform 1 :json-key "id")
   (name :initarg :name :type name :json-key "name")
   (type :initarg :type :type interaction-type :json-key "type")
   (resolved :initarg :resolved :type list :json-key "resolved")
   (options :initarg :options :type list :json-key "options")
   (guild-id :initarg :guild-id :type snowflake :json-key "guild_id")
   (target-id :initarg :target-id :type snowflake :json-key "target_id"))
  (:metaclass json-serializable-class))

(defclass interaction (json-encodable)
  ((id
    :initarg :id
    :type snowflake
    :json-key "id")
   (application-id
    :initarg :application-id
    :type snowflake
    :json-key "application_id")
   (type
    :initarg :type
    :type interaction-type
    :json-key "type")
   (data
    :initarg :data
    :type interaction-data
    :json-key "data")
   (guild
    :initarg :name-localizations
    :type guild
    :json-key "guild")
   (channel
    :initarg :channel
    :type string
    :json-key "channel")
   (channel-id
    :initarg :channel-id
    :type snowflake
    :json-key "channel_id")
   (member
    :initarg :member
    :type guild-member
    :json-key "member")
   (user
    :initarg :user
    :type user
    :json-key "user")
   (token
    :initarg :token
    :type string
    :json-key "token")
   (version
    :initarg :version
    :type integer
    :initform 1
    :json-key "version")
   (message
    :initarg :message
    :type message
    :json-key "message")
   (app-permissions
    :initarg :app-permissions
    :type string
    :json-key "app-permissions")
   (locale :initarg :locale :type string :json-key "locale")
   (guild-locale :initarg :guild-locale :type string :json-key "guild_locale")
   (entitlements :initarg :entitlements :type list :json-key "entitlements")
   (context
    :initarg :context
    :type integration-contexts
    :json-key "context"))
  (:metaclass json-serializable-class))

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

(defclass interaction-response (json-encodable)
  ((type :type interaction-type :initarg :type :initform 1 :json-key "type")
   (data :initarg :data :type interaction-callback :json-key "data"))
  (:metaclass json-serializable-class))

(defparameter ping (make-instance 'interaction-response :type 1))

(deftype command-type ()
  '(integer))

(defvar +command-chat-input+ 1)

(defclass application-command-data (json-encodable)
  ((id
    :initarg :id
    :type snowflake
    :initform nil)
   (type
    :initarg :type
    :type command-type)
   (resolved
    :initarg :resolved
    :type data)
   (options
    :initarg :options
    :type snowflake)
   (name
    :initarg :name
    :type string
    :initform nil))

  (:metaclass json-serializable-class))

(deftype command-type ()
  '(integer 1 4))

(deftype handler ()
  '(member 1 2))

(deftype integration ()
  '(member 0 1))

(deftype integration-context ()
  '(member 0 1 2))

(defclass command-choice (json-encodable)
  ((name :initarg :name :type string :json-key "name")
   (name-localizations
    :initarg :name-localizations
    :type list
    :json-key "name_localizations")
   (value :initarg :value :type t :json-key "value"))
  (:metaclass json-serializable-class))

(deftype option-type ()
  '(integer))
(defvar +option-string+ 3)
(defvar +option-boolean+ 5)

(defclass command-option (json-encodable)
  ((type :initarg :type :type option-type :json-key "type")
   (name :initarg :name :type name :json-key "name")
   (name-localizations :initarg :name-localizations :type list :json-key "name_localizations")
   (description :initarg :description :type string :json-key "description")
   (description-localizations)
   (required :initarg :required :type boolean :json-key "required" :json-type :bool)
   (choices :initarg :choices :type (list choice) :json-key "choices")
   (options :initarg :options :type (list command-option) :json-key "options")
   (channel-types :json-key "channel-types")
   (min-value :json-key "min-value")
   (max-value :json-key "max-value")
   (min-length :json-key "min-length")
   (max-length :json-key "max-length")
   (autocomplete :json-key "autocomplete"))
  (:metaclass json-serializable-class))

;; Parameters used to POST a new command to the discord API
;; Probably unnecessary to keep this separate
(defclass application-command-post (json-encodable)
  ((type
    :initarg :type
    :type command-type
    :json-key "type")
   (name
    :initarg :name
    :type string
    :json-key "name")
   (name-localizations
    :initarg :name-localizations
    :type list
    :json-key "name_localizations")
   (description
    :initarg :description
    :type string
    :json-key "description")
   (description-localizations
    :initarg :description-localizations
    :type string
    :json-key "description_localizations")
   (options
    :initarg :options
    :type (list command-option)
    :json-key "options")
   (default-member-permissions
    :initarg :default-member-permissions
    :type string
    :json-key "default_member_permissions")
   (nsfw
    :initarg :nsfw
    :type boolean
    :json-type :bool
    :json-key "nsfw")
   (integration-types
    :initarg :integration-types
    :type (list integration)
    :json-key "integration_types")
   (contexts
    :initarg :contexts
    :type (list integration-contexts)
    :json-key "contexts"))
  (:metaclass json-serializable-class))

;; https://discord.com/developers/docs/interactions/application-commands#application-command-object
(defclass application-command (application-command-post)
  ((id
    :initarg :id
    :type snowflake
    :json-key "id")
   (application-id
    :initarg :application-id
    :type snowflake
    :json-key "application_id")
   (guild-id
    :initarg :guild-id
    :type snowflake
    :json-key "guild_id")
   (version
    :initarg :version
    :type snowflake
    :initarg nil
    :json-key "version")
   (handler
    :initarg :handler
    :type handler
    :initform 1
    :json-key "handler"))

  (:metaclass json-serializable-class))
