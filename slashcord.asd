(asdf:defsystem "slashcord"
  :version "0.0.1"
  :author "Stefan Kuznetsov"
  :license "MIT"
  :depends-on ("alexandria" "ironclad" "hunchentoot" "easy-routes" "serapeum" "yason" "json-mop" "dexador" "log4cl")
  :serial t
  :components ((:module "src"
                :components
                ((:file "types")
                 (:file "api")
                 (:file "slashcord"))))
  :description "A library designed to handle webhook Discord interactions"
  :in-order-to ((asdf:test-op (asdf:test-op "slashcord/tests"))))

(asdf:defsystem "slashcord/tests"
  :author ""
  :license ""
  :depends-on ("slashcord"
               "fiveam")
  :serial t
  :components ((:module "tests"
                :components
                ((:file "types"))))
  :description "Test system for slashcord"
  :perform (asdf:test-op (o s)
                    (uiop:symbol-call :fiveam :run! 'slashcord/tests:all-tests)))

(asdf:defsystem "slashcord/hack"
  :author ""
  :license ""
  :depends-on ("slashcord")
  :serial t
  :components ((:module "hack"
                :components
                ((:file "create-commands"))))
  :description "One off scripts using the library")
