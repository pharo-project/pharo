testing
isRunning
	"Return true when I am running"
	
	^ self process notNil and: [ self serverSocket notNil ]