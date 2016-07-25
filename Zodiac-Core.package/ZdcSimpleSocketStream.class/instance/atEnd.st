testing
atEnd
	"I am atEnd when there is no more data to be read and there never will be.
	This means that readBuffer must be empty, there must be no more unread data 
	available at the socket, and the socket must be closed"
	
	readBuffer isEmpty ifFalse: [ ^ false ].
	(socket notNil and: [ self socketIsDataAvailable ]) 
		ifTrue: [
			"Assuming there really is data available, read it and recurse"
			self fillReadBufferNoWait.
			^ self atEnd ].   
	^ self isConnected not