testing
atEnd
	"I am atEnd when there is no more data to be read and there never will be.
	This means that readBuffer must be empty, there must be no more unread data 
	available at the socket, and the socket must be closed"
	
	readBuffer isEmpty ifFalse: [ ^ false ].
	socket notNil
		ifTrue: [
			"Try reading (there might stil be data in the SSL session) and test again"
			[ self fillReadBufferNoWait ]
				on: ConnectionClosed 
				do: [ ^ true ].
			readBuffer isEmpty ifFalse: [ ^ false ] ].   
	^ self isConnected not