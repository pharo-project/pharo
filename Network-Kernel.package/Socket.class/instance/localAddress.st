accessing
localAddress
	self isWaitingForConnection
		ifFalse: [[self waitForConnectionFor: Socket standardTimeout]
				on: ConnectionTimedOut
				do: [:ex | ^ ByteArray new: 4]].
	^ self primSocketLocalAddress: socketHandle