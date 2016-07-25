public
defaultOn: portNumber
	"Return the default instance on a given port,
	Keep a reference to it in a class instance variable.
	If there was no previous default instance, create a new one.
	If there was a previous default instance, reuse it:
	if it was running stop it, change the port if necessary.
	Delegate to the default server class."

	^ self defaultServerClass defaultOn: portNumber 