instance creation
tcpCreateIfFail: failBlock
	"Attempt to create a new socket. If successful, return the new socket. Otherwise, return the result of evaluating the given block. Socket creation can fail if the network isn't available or if there are not sufficient resources available to create another socket."

	| sock |
	self initializeNetwork.
	sock := self newTCP.
	sock isValid ifFalse: [^ failBlock value].
	^ sock
