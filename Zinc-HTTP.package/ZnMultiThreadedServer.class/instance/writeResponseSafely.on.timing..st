request handling
writeResponseSafely: response on: stream timing: timing
	"Write response to stream, when the connection is closed or times out we ignore this"

	[ self writeResponse: response on: stream timing: timing ]
		on: self writeResponseTerminationExceptionSet
		do: [ :exception | self logServerWriteError: exception ]