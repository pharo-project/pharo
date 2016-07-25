initialization
startUp: isImageStarting
	ChangesLog default.
	isImageStarting ifTrue: [Smalltalk openSourceFiles]