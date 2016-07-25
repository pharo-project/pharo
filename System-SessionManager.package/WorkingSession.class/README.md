I am the unique object for the current smalltalk session (I.e. between a startup and a shutdown).
On each image startup the current session is invalidated and a new session is created.
I define how errors should be handled.

ps: I replace the old Session class that was only used as a session identifier  