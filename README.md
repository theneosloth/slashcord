# Slashcord

Slashcord is a minimal framework to handle Discord [application commands](https://discord.com/developers/docs/interactions/application-commands).

Currently only a small subset of the API is implemented.

Inspired by https://github.com/JohnnyJayJay/slash/

# Structure
The library contains a set of json-encodable CLOS classes representing potential Interaction callbacks and responses
It's organized into 3 main packages:

## slashcord/types
Contains mapping from CLOS objects to Discord API json objects. All types descend from a utility class json-encodable and are expected to have all field types recorded with json-mop.

## slashcord/rest
A minimal implementation of a Discord API rest client, used only to register, list and delete commands with the API

## slashcord/server
A web server that implements all the authentication required for receiving Discord Interactions as documented [here](https://discord.com/developers/docs/interactions/receiving-and-responding).
By default it responds only to Ping interactions. Additional handlers can be registered through slashcord/server:define-handler


# Example Bot

``` common-lisp
(defpackage ping-bot
  (:use :cl)
  (:import-from :slashcord/types :make-command :make-option)
  (:import-from :slashcord/rest :make-client :create-command)
  (:import-from :slashcord/server :main :define-handler))

(in-package :ping-bot)

(defvar +bot-token+ (uiop:getenv "SLASHCORD_BOT_TOKEN"))
(defvar +bot-id+ (uiop:getenv "SLASHCORD_APPLICATION_ID"))
(defvar +server-public-key+ (uiop:getenv "SLASHCORD_PUBLIC_KEY"))

(slashcord/server:define-handler :command (interaction)
  "Return a Pong! message.
   Note that this is a handler for all commands received, per-command dispatching would have to be implemented manually.
   All handlers receive a slashcord/types:interaction and are expected to return a slashcord/types:interaction-callback"
  (declare (ignorable interaction))
  (let* ((pong-response (slashcord/types:make-text-callback "Pong!")))
    pong-response))

(defun create-ping-command ()
  "Register a ping command on the discord API."
  (let ((client (make-client +bot-id+ +bot-token+))
        (command (make-command "ping" "Send a Pong!")))
    (create-command client ping-command)))

(defun main ()
  (create-ping-command)
  (list-commands)
  (main))
```
