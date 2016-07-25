private protocol
readResponse
	self signalHTTPProgress: 'Reading response'.
	response := (#(HEAD CONNECT) includes: request method)
		ifTrue: [ ZnResponse readHeaderFrom: connection ]
		ifFalse: [ 
			self streaming
				ifTrue: [ ZnResponse readStreamingFrom: connection ]
				ifFalse: [ ZnResponse readFrom: connection ] ].
	lastUsed := Time totalSeconds