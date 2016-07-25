stream in
upToEnd
	"Answer all data coming in on the socket until the socket
	is closed by the other end, or we get a timeout.
	This means this method catches ConnectionClosed by itself.
	
	NOTE: Does not honour timeouts if shouldSignal is false!"

	[[self isConnected] whileTrue: [self receiveData]]
		on: ConnectionClosed
		do: [:ex | "swallow it"]. 
	^self nextAllInBuffer