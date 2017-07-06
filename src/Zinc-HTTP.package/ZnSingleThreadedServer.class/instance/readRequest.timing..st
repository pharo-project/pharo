request handling
readRequest: stream timing: timing
	"Read a request from stream.
	We add a virtual header containing the remote IP address of the client"

	| request initialMilliseconds |
	initialMilliseconds := Time millisecondClockValue.
	request := self withMaximumEntitySizeDo: [ 
		self withMaximumNumberOfDictionaryEntriesDo: [ 
			self withDefaultCharacterEncoderDo: [
				self reader value: stream ] ] ].
	request headers 
		at: ZnConstants remoteAddressHeader
		put: (ZnNetworkingUtils ipAddressToString: stream socket remoteAddress).
	timing requestDuration:  (self logRequestRead: request started: initialMilliseconds).
	^ request