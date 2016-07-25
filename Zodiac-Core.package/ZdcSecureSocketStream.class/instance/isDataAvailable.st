testing
isDataAvailable
	"Return true when there is data available for reading.
	This does not block."
	
	readBuffer isEmpty ifFalse: [ ^ true ].
	socket notNil
		ifTrue: [
			"Try reading (there might stil be data in the SSL session) and test again"
			[ self fillReadBufferNoWait ]
				on: ConnectionClosed 
				do: [ ^ false ].
			readBuffer isEmpty ifFalse: [ ^ true ] ].   
	^ false