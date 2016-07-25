public
startOn: portNumber
	"Instanciate and return a new listener on a given port and start listening."

	^ (self on: portNumber) 
		start;
		yourself