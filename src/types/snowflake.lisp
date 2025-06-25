(in-package :slashcord/types)

(deftype snowflake () `(integer 0 (,most-positive-fixnum)))

(-> snowflake (string) snowflake)
(defun snowflake (timestamp)
  (coerce (parse-integer timestamp) 'snowflake))
