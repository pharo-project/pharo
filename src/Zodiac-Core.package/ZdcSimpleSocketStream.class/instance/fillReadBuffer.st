private in
fillReadBuffer
	"Ask the socket to fill the read buffer with data. Wait for a data."
	
	self fillReadBufferNoWait.
	(readBuffer isEmpty and: [ self isConnected ])
		ifTrue: [ 
			[ self socketWaitForData ] on: ConnectionClosed do: [ ^ self ]. "when successful, recurse, else signal exception"
			self fillReadBuffer ]  