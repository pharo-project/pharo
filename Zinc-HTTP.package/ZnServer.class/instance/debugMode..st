options
debugMode: boolean
	"Set my debug mode, the default being false.
	In debug mode, Smalltalk Error during #handleRequest: will raise a Debugger.
	When not in debug mode, a Smalltalk Error during #handleRequest: will result in an HTTP Server Error response."
	
	^ self optionAt: #debugMode put: boolean