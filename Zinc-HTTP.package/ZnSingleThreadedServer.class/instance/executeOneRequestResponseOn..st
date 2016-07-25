request handling
executeOneRequestResponseOn: stream
	"Execute one single HTTP request / response cycle on stream in 3 steps:
	#readRequest: #handleRequest: and #writeResponse:on:
	Indicate that we intend to close the connection afterwards"

	| request response timing|
	ZnCurrentServer
		value: self
		during: [ 
			timing := ZnServerTransactionTiming new.
			request := self readRequest: stream timing: timing.
			response := self handleRequest: request timing: timing.
			response setConnectionClose.
			self writeResponse: response on: stream timing: timing.
			self logServerTransactionRequest: request response: response timing: timing ]