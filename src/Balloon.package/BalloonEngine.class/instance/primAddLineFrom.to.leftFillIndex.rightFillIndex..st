primitives-adding
primAddLineFrom: start to: end leftFillIndex: leftFillIndex rightFillIndex: rightFillIndex
	<primitive: 'primitiveAddLine' module: 'B2DPlugin'>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddLineFrom: start to: end leftFillIndex: leftFillIndex rightFillIndex: rightFillIndex
	].
	^self primitiveFailed