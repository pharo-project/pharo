public
startDefaultOn: portNumber
	"Start and return the default instance on a given port.
	Keep a reference to it in a class instance variable.
	If there was no previous default instance, create a new one.
	If there was a previous default instance, reuse it:
	if it was running stop and start it, effectively restarting it.
	Change the port if necessary.
	Starting the default server will register it automatically."

	^ (self defaultOn: portNumber) 
		start; 
		yourself