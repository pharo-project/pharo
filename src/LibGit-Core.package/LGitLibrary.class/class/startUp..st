system startup
startUp: isImageStarting

	isImageStarting
		ifTrue: [ Initialized := false ]