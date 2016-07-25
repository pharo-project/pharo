instance creation
openConnectionToHost: hostIP port: portNumber
	^ self openConnectionToHost: hostIP port: portNumber timeout: Socket standardTimeout
