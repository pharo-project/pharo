instance creation
openOnHostNamed: hostName port: portNumber
	| serverIP |
	serverIP := NetNameResolver addressForName: hostName timeout: 20.
	^self openOnHost: serverIP port: portNumber
