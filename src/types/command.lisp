(in-package :slashcord/types)
(deftype command-type ()
  '(integer))

(defvar +command-chat-input+ 1)

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
    :json-key "name_localizations"
    :json-type (:list :string))
   (value :initarg :value :type t :json-key "value"))
  (:metaclass json-serializable-class))

(defvar +option-string+ 3)
(defvar +option-integer+ 3)
(defvar +option-boolean+ 5)

(deftype option-type ()
         `(member ,+option-string+ ,+option-integer+ ,+option-boolean+))

(defclass command-option (json-encodable)
  ((type :initarg :type :type option-type :json-key "type")
   (name :initarg :name :type name :json-key "name")
   (name-localizations :initarg :name-localizations :type list :json-key "name_localizations")
   (description :initarg :description :type string :json-key "description")
   (description-localizations :initarg :description-localizations :json-key "description_localizations")
   (required :initarg :required :type boolean :json-key "required" :json-type :bool)
   (choices :initarg :choices :type (list command-choice) :json-type (list command-choice) :json-key "choices")
   (options :initarg :options :type (list command-option) :json-type (list command-option) :json-key "options")
   (channel-types :initarg :channel-types :json-key "channel_types")
   (min-value :initarg :min-value :json-key "min_value")
   (max-value :initarg :max-value :json-key "max_value")
   (min-length :initarg :min-length :json-key "min_length")
   (max-length :initarg :max-length :json-key "max_length")
   (autocomplete :initarg :autocomplete :json-key "autocomplete"))
  (:metaclass json-serializable-class))

(deftype make-option-param ()
  '(member :string :integer :boolean))

(-> make-option (string make-option-param &key (:description string) (:required boolean) (:choices list) (:options list)) t)
(defun make-option (name type &key description required choices options)
  (let* ((types (list :string +option-string+
                        :integer +option-integer+
                        :boolean +option-boolean+))
         (type-val (getf types type +option-string+)))
    (make-instance 'command-option
                   :type type-val
                   :name name
                   :description description
                   :required required
                   :choices (or choices #())
                   :options (or options #()))))

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
    :initarg :options)
   (name
    :initarg :name
    :type string
    :initform nil))

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


(-> make-command (string string &key (:options (or list vector))) t)
(defun make-command (name description &key options)
  (make-instance 'application-command-post
                 :name name
                 :description description
                 :type 1
                 :options (or options #())))

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
