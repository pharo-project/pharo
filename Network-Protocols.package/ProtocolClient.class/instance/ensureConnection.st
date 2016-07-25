private
ensureConnection
	self isConnected
		ifTrue: [^self].
	self stream
		ifNotNil: [self stream close].

	self stream: (SocketStream openConnectionToHost: self host port: self port).
	self checkResponse.
	self login