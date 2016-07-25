class initialization
installPrimaryBytecodeSet: aBytecodeEncoderSubclass
	PrimaryBytecodeSetEncoderClass == aBytecodeEncoderSubclass ifTrue:
		[ ^self ].
	self checkIsValidBytecodeEncoder: aBytecodeEncoderSubclass.
	self checkBytecodeSetConflictsInMethodsWith: [:m| 
		m usesPrimaryBytecodeSet and: [m encoderClass ~~ aBytecodeEncoderSubclass]].
	PrimaryBytecodeSetEncoderClass := aBytecodeEncoderSubclass