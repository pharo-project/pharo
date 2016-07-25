class initialization
installSecondaryBytecodeSet: aBytecodeEncoderSubclass
	PrimaryBytecodeSetEncoderClass == aBytecodeEncoderSubclass ifTrue:
		[ ^ self ].
	self checkIsValidBytecodeEncoder: aBytecodeEncoderSubclass.
	self checkBytecodeSetConflictsInMethodsWith: [ :m | 
		m usesSecondaryBytecodeSet and: [ m encoderClass ~~ aBytecodeEncoderSubclass ] ].
	SecondaryBytecodeSetEncoderClass := aBytecodeEncoderSubclass