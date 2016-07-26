primitives-adding
primAddPolygon: points segments: nSegments fill: fillStyle lineWidth: lineWidth lineFill: lineFill
	<primitive: 'primitiveAddPolygon' module: 'B2DPlugin'>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddPolygon: points segments: nSegments fill: fillStyle lineWidth: lineWidth lineFill: lineFill
	].
	^self primitiveFailed