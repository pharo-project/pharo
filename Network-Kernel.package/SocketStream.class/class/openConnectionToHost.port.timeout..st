instance creation
openConnectionToHost: hostIP port: portNumber timeout: timeout
	| socket |
	socket := Socket new.
	socket connectTo: hostIP port: portNumber waitForConnectionFor: timeout.
	^self on: socket