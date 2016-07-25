public
startOn: portNumber
	"Instanciate and return a new listener on a given port and start listening.
	Delegate to the default server class."

	^ self defaultServerClass startOn: portNumber 