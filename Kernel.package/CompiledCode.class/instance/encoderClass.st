accessing
encoderClass
	^ self signFlag 
		ifTrue: [ SecondaryBytecodeSetEncoderClass ]
		ifFalse: [ PrimaryBytecodeSetEncoderClass ]