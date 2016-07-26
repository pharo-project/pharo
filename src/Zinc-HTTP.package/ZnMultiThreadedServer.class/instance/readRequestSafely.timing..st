request handling
readRequestSafely: stream timing: timing
	"Read request from stream, returning nil when the connection is closed or times out"

	^ [ 
		[ self readRequest: stream timing: timing ]
			on: self readRequestBadExceptionSet
			do: [ :exception | 
				self logServerReadError: exception.
				self writeResponseBad: exception on: stream timing: timing.
				nil ] ]
			on: self readRequestTerminationExceptionSet
			do: [ :exception | 
				self logServerReadError: exception.
				nil ]