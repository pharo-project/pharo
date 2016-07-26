primitives-adding
primAddBitmapFill: form colormap: cmap tile: tileFlag from: origin along: direction normal: normal xIndex: xIndex
	<primitive: 'primitiveAddBitmapFill' module: 'B2DPlugin'>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddBitmapFill: form colormap: cmap tile: tileFlag from: origin along: direction normal: normal xIndex: xIndex
	].
	^self primitiveFailed