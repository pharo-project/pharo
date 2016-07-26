primitives-adding
primAddGradientFill: colorRamp from: origin along: direction normal: normal radial: isRadial
	<primitive: 'primitiveAddGradientFill' module: 'B2DPlugin'>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddGradientFill: colorRamp 
				from: origin 
				along: direction 
				normal: normal 
				radial: isRadial
	].
	^self primitiveFailed