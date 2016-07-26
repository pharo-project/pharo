system startup
startUp: isImageStarting
	"Pharo is starting up. If this platform requires specific intialization, this is a great place to put it."
	isImageStarting
		ifTrue: [Default := nil]