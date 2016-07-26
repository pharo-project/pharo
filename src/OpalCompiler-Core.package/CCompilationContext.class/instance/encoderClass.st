accessing
encoderClass
	^ encoderClass ifNil: [ encoderClass := BytecodeBackend ]