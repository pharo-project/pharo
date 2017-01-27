accessing
localPort
	"If in the process of connecting, wait for connection to be established and binding to address completed before resolving."

	^ [ self primSocketLocalPort: socketHandle ]
		on: Error
		do: [ :e | 
			self isWaitingForConnection
				ifTrue: [ self waitForConnectionFor: Socket standardTimeout ifTimedOut: nil.
					self primSocketLocalPort: socketHandle ]
				ifFalse: [ e pass ] ]