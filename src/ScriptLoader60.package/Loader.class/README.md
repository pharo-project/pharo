I'm an abstract class which collect some infos about package and system (versions, dirty).
#currentChangedPackages instance method and #currentMajorVersionNumber class are missing and provided by my wellknown subclass ScriptLoader (see ScriptLoader comment).


Stef: This is a class that was extracted from scriptLoader because I want to slowly get in place a loader infrastructure based on Metacello.