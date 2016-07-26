primitives-adding
primAddExternalEdge: index initialX: initialX initialY: initialY initialZ: initialZ leftFillIndex: leftFillIndex rightFillIndex: rightFillIndex
	<primitive: 'primitiveRegisterExternalEdge' module: 'B2DPlugin'>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddExternalEdge: index initialX: initialX initialY: initialY initialZ: initialZ leftFillIndex: leftFillIndex rightFillIndex: rightFillIndex
	].
	^self primitiveFailed