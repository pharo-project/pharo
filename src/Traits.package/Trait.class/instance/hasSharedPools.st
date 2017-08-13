pool variables
hasSharedPools
	"Returns whether the receiver uses shared pools directly (Does not take into account that it may inherit shared pool uses."
	^ self sharedPools notEmpty