options
timeout
	"Return the timeout in seconds for network socket stream operations
	like connecting, reading and writing. Defaults to a global setting."
	
	^ self optionAt: #timeout ifAbsent: [ ZnNetworkingUtils defaultSocketStreamTimeout ]