(asdf:defsystem #:slashcord
  :version "0.0.1"
  :author "Stefan Kuznetsov"
  :license "MIT"
  :depends-on ("alexandria" "ironclad" "hunchentoot" "easy-routes" "serapeum" "yason" "json-mop" "dexador")
  :serial t
  :components ((:module "src"
                :components
                ((:module "types"
                  :components
                  ((:file "package")
                   (:file "encodable")
                   (:file "snowflake")
                   (:file "interaction-types")
                   (:file "channel")
                   (:file "resolved")
                   (:file "interaction-data")
                   (:file "interaction")
                   (:file "interaction-callback-data")
                   (:file "interaction-callback")
                   (:file "command")
                   (:file "util")))
                 (:file "client")
                 (:file "server"))))
  :description "A library for handling Discord webhook interactions"
  :in-order-to ((asdf:test-op (asdf:test-op "slashcord/tests")))
  :build-operation "program-op"
  :entry-point "slashcord/server:main"
  :build-pathname "slashcord")


(asdf:defsystem #:slashcord/examples
  :author ""
  :license ""
  :depends-on ("slashcord")
  :serial t
  :components ((:module "examples"
                :components
                ((:file "ping")
                 (:file "lfg"))))
  :description "One off scripts using the library")

(asdf:defsystem #:slashcord/tests
  :author ""
  :license ""
  :depends-on (:slashcord :fiveam)
  :serial t
  :components ((:module "tests"
                :components
                ((:file "types"))))
  :description "Test system for slashcord"
  :perform (asdf:test-op (o s)
                    (uiop:symbol-call :fiveam '#:run! :all-tests)))
