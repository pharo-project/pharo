accessing
localPort
	self isWaitingForConnection
		ifFalse: [[self waitForConnectionFor: Socket standardTimeout]
				on: ConnectionTimedOut
				do: [:ex | ^ 0]].
	^ self primSocketLocalPort: socketHandle