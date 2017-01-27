accessing
remoteAddress
	"If in the process of connecting, wait for connection to be established and binding to address completed before resolving."

	^ [ self primSocketRemoteAddress: socketHandle ]
		on: Error
		do: [ :e | 
			self isWaitingForConnection
				ifTrue: [ self waitForConnectionFor: Socket standardTimeout ifTimedOut: nil.
					self primSocketRemoteAddress: socketHandle ]
				ifFalse: [ e pass ] ]