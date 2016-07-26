instance creation
createIfFail: failBlock
	"Attempt to create a new socket. If successful, return the new socket. Otherwise, return the result of evaluating the given block. Socket creation can fail if the network isn't available or if there are not sufficient resources available to create another socket."
	"Note: The default creates a TCP socket"
	^self tcpCreateIfFail: failBlock