class initialization
checkIsValidBytecodeEncoder: aBytecodeEncoderSubclass
	(aBytecodeEncoderSubclass inheritsFrom: OpalBytecodeEncoder) ifFalse:
		[self error: 'A bytecode set encoder is expected to be a subclass of BytecodeEncoder'].