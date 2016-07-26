drawing
drawRectangle: rect fill: fillStyle borderWidth: borderWidth borderColor: borderColor transform: aMatrix

	| fills |
	self edgeTransform: aMatrix.
	self resetIfNeeded.
	fills := self registerFill: fillStyle and: borderColor.
	self primAddRectFrom: rect origin 
			to: rect corner
			fillIndex: (fills at: 1)
			borderWidth: borderWidth
			borderColor: (fills at: 2).
	self postFlushIfNeeded.