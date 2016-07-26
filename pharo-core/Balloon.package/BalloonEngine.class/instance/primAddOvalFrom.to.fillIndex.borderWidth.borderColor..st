primitives-adding
primAddOvalFrom: start to: end fillIndex: fillIndex borderWidth: width borderColor: pixelValue32
	<primitive: 'primitiveAddOval' module: 'B2DPlugin'>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddOvalFrom: start to: end fillIndex: fillIndex borderWidth: width borderColor: pixelValue32
	].
	^self primitiveFailed