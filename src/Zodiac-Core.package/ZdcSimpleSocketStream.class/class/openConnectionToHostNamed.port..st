instance creation
openConnectionToHostNamed: hostName port: portNumber
	| platformSocket socketStream hostIP |
	platformSocket := Socket newTCP.
	socketStream := self on: platformSocket.
	hostIP := NetNameResolver addressForName: hostName timeout: socketStream timeout.
	^ socketStream 
		connectTo: hostIP port: portNumber;
		yourself