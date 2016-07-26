instance creation
headerFlagForEncoder: anEncoderClass
	anEncoderClass == PrimaryBytecodeSetEncoderClass ifTrue: [ ^ 0 ].
	anEncoderClass == SecondaryBytecodeSetEncoderClass ifTrue: [ ^ SmallInteger minVal ].
	
	"The following is a hack to support the old compiler... We can remove it once the old compiler is removed"
	(PrimaryBytecodeSetEncoderClass name endsWith: anEncoderClass name) ifTrue: [ ^ 0 ].
	(SecondaryBytecodeSetEncoderClass name endsWith: anEncoderClass name) ifTrue: [ ^ SmallInteger minVal ].
	
	self error: 'The encoder is not one of the two installed bytecode sets'.
	^ 0