testing
shouldInitializeVariables
	"We should initialize variables if: 1) the platform has changed (for example, if this image was now started in a different OS) or if 2) the class variables where never initialized before."
	^ self hasAlreadyBeenInitializedBefore not or: [ self didPlatformChange ]