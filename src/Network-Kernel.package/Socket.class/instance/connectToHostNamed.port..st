connection open/close
connectToHostNamed: hostName port: portNumber
	| serverIP |
	serverIP := NetNameResolver addressForName: hostName timeout: 20.
	^self connectTo: serverIP port: portNumber
