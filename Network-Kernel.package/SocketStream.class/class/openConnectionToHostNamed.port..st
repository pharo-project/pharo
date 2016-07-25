instance creation
openConnectionToHostNamed: hostName port: portNumber
	| hostIP |
	hostIP := NetNameResolver addressForName: hostName timeout: 20.
	^self openConnectionToHost: hostIP port: portNumber