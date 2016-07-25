public
on: portNumber
	"Instantiate a new listener on a given port,
	send #start to it to start listening.
	Delegate to the default server class."

	^ self defaultServerClass on: portNumber 