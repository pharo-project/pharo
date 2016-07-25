startup - shutdown
start: isImageStarting
	Smalltalk vm setGCParameters.
	isImageStarting
		ifTrue: [ Smalltalk clearExternalObjects ].
	
	self runStartup: isImageStarting.