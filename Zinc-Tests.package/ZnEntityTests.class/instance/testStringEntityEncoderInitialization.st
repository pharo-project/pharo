testing
testStringEntityEncoderInitialization
	| entity response |
	entity := ZnEntity text: 'Regression test'.
	self assert: entity hasEncoder not.
	response := ZnResponse ok: entity.
	self assert: entity hasEncoder.
	ByteArray streamContents: [ :stream | response writeOn: stream ].
	self assert: entity hasEncoder
	