testing
isListening
	"Return true when I have a valid server socket listening at the correct port"
	
	^ self serverSocket notNil
		and: [ self serverSocket isValid and: [ self serverSocket localPort = self port ] ]