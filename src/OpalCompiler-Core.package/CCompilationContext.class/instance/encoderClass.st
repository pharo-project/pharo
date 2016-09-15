accessing
encoderClass
	^ encoderClass ifNil: [ encoderClass := self class bytecodeBackend ]