system startup
startUp: isImageStarting

	isImageStarting ifTrue: [ self reset ]