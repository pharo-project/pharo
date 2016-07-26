primitives-adding
primAddBezierShape: points segments: nSegments fill: fillStyle lineWidth: lineWidth lineFill: lineFill
	<primitive: 'primitiveAddBezierShape' module: 'B2DPlugin'>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddBezierShape: points segments: nSegments fill: fillStyle lineWidth: lineWidth lineFill: lineFill
	].
	^self primitiveFailed