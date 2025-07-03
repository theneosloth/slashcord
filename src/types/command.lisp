(in-package :slashcord/types)

(defvar +command-chat-input+ 1)
(deftype command-type ()
  `(member ,+command-chat-input+))

(deftype integration ()
  '(member 0 1))

(deftype integration-context ()
  '(member 0 1 2))

(defclass command-choice (json-encodable)
  ((name :initarg :name :type string :json-key "name")
   (name-localizations
    :initarg :name-localizations
    :type list
    :json-key "name_localizations"
    :json-type (:list :string))
   (value :initarg :value :json-key "value"))
  (:metaclass json-serializable-class))

(defvar +option-string+ 3)
(defvar +option-integer+ 4)
(defvar +option-boolean+ 5)
(defvar +option-user+ 6)
(defvar +option-channel+ 7)
;;TODO: rest of options

(deftype option-type ()
         `(member ,+option-string+ ,+option-integer+ ,+option-boolean+ ,+option-user+ ,+option-channel+))

(defclass command-option (json-encodable)
  ((type :initarg :type :type option-type :json-key "type")
   (name :initarg :name :type name :json-key "name")
   (name-localizations :initarg :name-localizations :type list :json-key "name_localizations")
   (description :initarg :description :type string :json-key "description")
   (description-localizations :initarg :description-localizations :json-key "description_localizations")
   (required :initarg :required :type boolean :json-key "required" :json-type :bool)
   (choices :initarg :choices :type (vector command-choice) :json-type (:vector command-choice) :json-key "choices")
   (options :initarg :options :type (vector command-option) :json-type (:vector command-option) :json-key "options")
   (channel-types :initarg :channel-types :json-key "channel_types")
   (min-value :initarg :min-value :json-key "min_value")
   (max-value :initarg :max-value :json-key "max_value")
   (min-length :initarg :min-length :json-key "min_length")
   (max-length :initarg :max-length :json-key "max_length")
   (autocomplete :initarg :autocomplete :json-key "autocomplete"))
  (:metaclass json-serializable-class))

(defclass application-command-data (json-encodable)
  ((id
    :initarg :id
    :type snowflake
    :initform nil
    :json-key "id")
   (type
    :initarg :type
    :type command-type
    :json-key "type")
   (resolved
    :initarg :resolved
    :type resolved
    :json-type resolved
    :json-key "resolved")
   (options
    :initarg :options
    :json-key "options")
   (name
    :initarg :name
    :type string
    :initform nil
    :json-key "name"))

  (:metaclass json-serializable-class))

;; Parameters used to POST a new command to the discord API
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
    :type (vector command-option)
    :json-type (:vector command-option)
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
    :type (vector integration-types)
    :json-type (:vector integration-types)
    :json-key "integration_types")
   (contexts
    :initarg :contexts
    :type (vector integration-context)
    :json-type (:vector integration-contexts)
    :json-key "contexts"))
  (:metaclass json-serializable-class))

(deftype handler ()
  '(member 1 2))

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
