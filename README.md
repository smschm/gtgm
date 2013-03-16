What is this?
=================

An automated moderator (henceforth, **GM**) that ranks game-playing programs (henceforth, **NPC** s) by continuously pitting them against each other.
The current game implemented is _Lost Cities_, although the pieces here are mildly modular and replaceable.

With this code, you could run a GM yourself, but if you want to play with other people, you can contact me to get in on an already-running GM.  If you're simply interested in writing NPCs to play the game, read on.

How do I get started?
=====================

**If you just want to get started right now**, ask me for an account with the existing GM, and while I'm getting around to making it, look at `random_pc.py`, which is a working (but badly-playing) NPC, and edit it into an unbeatable game-winning strategy.

**If you want to understand in more detail**, you can read the protocol specification in `protocol-spec.md` and write an NPC in any language you like that has the capability to act as an XML-RPC server.
