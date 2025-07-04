(in-package :slashcord/types)

(defclass channel (json-encodable)
  ((id :initarg :id :type snowflake :initform 1 :json-key "id")
   (type :initarg :type :type interaction-type :json-key "type")
   (guild-id :initarg :guild-id :type snowflake :json-key "guild_id")
   (position :initarg :position :json-key "position")
   (permission-overwrites :initarg :permission-overwrites :json-key "permission_overwrites")
   (name :initarg :name :type string :json-key "name"))
  (:metaclass json-serializable-class))
