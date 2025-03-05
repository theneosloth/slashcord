SHELL := /bin/sh
LISP ?= sbcl

.PHONY: *

build:
	sbcl --non-interactive \
	--eval "(require 'asdf)" \
	--load "slashcord.asd" \
	--eval "(asdf:load-system :slashcord)" \
	--eval "(asdf:make :slashcord)"

test:
	sbcl --non-interactive \
	--eval "(load (sb-ext:posix-getenv \"ASDF\"))" \
	--load "slashcord.asd" \
	--eval "(asdf:test-system :slashcord)"

share:
	zrok share public http://localhost:4242
