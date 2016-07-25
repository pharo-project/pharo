instance creation
openConnectionToHost: hostIP port: portNumber
	| platformSocket |
	platformSocket := Socket newTCP.
	^ (self on: platformSocket)
		connectTo: hostIP port: portNumber;
		yourself