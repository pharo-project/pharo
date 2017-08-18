request handling
writeResponse: response on: stream timing: timing
	"Write response to stream and flush the stream"
	
	| initialMilliseconds |
	initialMilliseconds := Time millisecondClockValue.
	self withDefaultCharacterEncoderDo: [ response writeOn: stream ].
	stream flush.
	timing responseDuration: (self logResponseWritten: response started: initialMilliseconds)  