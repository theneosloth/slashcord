(defpackage slashcord/types
  (:use :cl)
  (:import-from :serapeum :-> :dict)
  (:import-from :easy-routes :defroute)
  (:import-from :json-mop :json-serializable-class)
  (:export
   :application-command-post
   :to-json
   :from-json
   :make-option
   :make-command))

(in-package :slashcord/types)

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

(deftype interaction-type ()
  `(member thing ,@+interaction-types+))

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

;; https://discord.com/developers/docs/interactions/receiving-and-responding#interaction-object-application-command-interaction-data-option-structure
(defclass interaction-resolved (json-encodable)
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

(defclass channel (json-encodable)
  ((id :initarg :id :type snowflake :initform 1 :json-key "id")
   (type :initarg :type :type interaction-type :json-key "type")
   (guild-id :initarg :guild-id :type snowflake :json-key "guild_id")
   (position :initarg :position :json-key "position")
   (permission-overwrites :initarg :permission-overwrites :json-key "permission_overwrites")
   (name :initarg :name :type string :json-key "name"))
  (:metaclass json-serializable-class))

(defclass interaction-data (json-encodable)
  ((id :initarg :id :type snowflake :initform 1 :json-key "id")
   (name :initarg :name :type name :json-key "name")
   (type :initarg :type :type interaction-type :json-key "type")
   (resolved :initarg :resolved :type interaction-resolved :json-type interaction-resolved :json-key "resolved")
   (options :initarg :options :type command-options :json-key "options")
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
    :json-type interaction-data
    :json-key "data")
   (guild
    :initarg :name-localizations
    :type guild
    :json-key "guild")
   (channel
    :initarg :channel
    :type channel
    :json-type channel
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
  ((type :type interaction-type :initarg :type :initform +pong-callback+ :json-key "type")
   (data :initarg :data :type interaction-callback :json-type interaction-callback :json-key "data"))
  (:metaclass json-serializable-class))

(defparameter pong (make-instance 'interaction-response :type +pong-callback+))

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
    :initarg :options)
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
