I am a basic class of OSEvent hierarchy.
The specific events are coming from operating system and converted to corresponding OSEvent subinstance(s) in order to handle them.

Events can implement a default action, which will be performed after dispatch on event handling,
unless they are suppressed using #suppressDefaultAction message.