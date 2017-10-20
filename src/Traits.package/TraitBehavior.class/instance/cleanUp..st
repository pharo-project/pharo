cleanup
cleanUp: aggressive
	"Clean out any caches and other state that should be flushed when trying to get an image into a pristine state. The argument should be used to indicate how aggressive the cleanup should be. Some subclasses may act differently depending on its value - for example, ChangeSet will only delete all unused and reinitialize the current change set if we're asking it to be aggressive."

	^self cleanUp