# Slashcord

Slashcord is a minimal framework to handle Discord [application commands](https://discord.com/developers/docs/interactions/application-commands).

Currently only the minimal subset of the API is implemented.

Inspired by https://github.com/JohnnyJayJay/slash/

## Implementation details

All responses are encoded as json-mop CLOS objects. Fields that are optional are left as unbound slots, fields that are required are initialized to nil so that an error is thrown on decode.
