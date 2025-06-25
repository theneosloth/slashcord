(in-package :slashcord/types)

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
