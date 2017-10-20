private protocol
readResponse
	self signalHTTPProgress: 'Reading response'.
	self withMaximumNumberOfDictionaryEntriesDo: [
		self withDefaultCharacterEncoderDo: [
			response := (#(HEAD CONNECT) includes: request method)
				ifTrue: [ ZnResponse readHeaderFrom: connection ]
				ifFalse: [ 
					self streaming
						ifTrue: [ ZnResponse readStreamingFrom: connection ]
						ifFalse: [ 
							self withMaximumEntitySizeDo: [ ZnResponse readFrom: connection ] ] ] ] ].
	lastUsed := Time totalSeconds