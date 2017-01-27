accessing
remotePort
	"If in the process of connecting, wait for connection to be established and binding to address completed before resolving."

	^ [ self primSocketRemotePort: socketHandle ]
		on: Error
		do: [ :e | 
			self isWaitingForConnection
				ifTrue: [ self waitForConnectionFor: Socket standardTimeout ifTimedOut: nil.
					self primSocketRemotePort: socketHandle ]
				ifFalse: [ e pass ] ]