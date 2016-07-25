primitives-adding
primAddExternalFill: index
	<primitive: 'primitiveRegisterExternalFill' module: 'B2DPlugin'>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddExternalFill: index
	].
	^self primitiveFailed