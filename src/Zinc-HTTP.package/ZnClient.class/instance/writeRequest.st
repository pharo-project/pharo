private protocol
writeRequest
	self signalHTTPProgress: 'Writing request'.
	self withDefaultCharacterEncoderDo: [ request writeOn: connection ].
	connection flush