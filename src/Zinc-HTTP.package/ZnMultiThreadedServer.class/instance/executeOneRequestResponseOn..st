request handling
executeOneRequestResponseOn: stream
	"Execute one HTTP request / response cycle on stream in 3 steps
	#readRequest: #handleRequest: and #writeResponse:on:
	Return true when the outer loop we are in should stop."

	| request response timing|
	timing := ZnServerTransactionTiming new.
	(request := self readRequestSafely: stream timing: timing)
		ifNil: [ ^ true ]
		ifNotNil: [ 
			response := self handleRequest: request timing: timing.
			self augmentResponse: response forRequest: request.
			self writeResponseSafely: response on: stream timing: timing.
			self logServerTransactionRequest: request response: response timing: timing.
			response useConnection: stream ].
	^ request wantsConnectionClose or: [ response wantsConnectionClose ]