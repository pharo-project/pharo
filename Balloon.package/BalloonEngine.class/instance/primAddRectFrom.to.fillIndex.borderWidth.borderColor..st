primitives-adding
primAddRectFrom: start to: end fillIndex: fillIndex borderWidth: width borderColor: pixelValue32
	<primitive: 'primitiveAddRect' module: 'B2DPlugin'>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddRectFrom: start to: end fillIndex: fillIndex borderWidth: width borderColor: pixelValue32
	].
	^self primitiveFailed