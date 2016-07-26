drawing
drawBezierShape: points fill: fillStyle borderWidth: borderWidth borderColor: borderFill transform: aTransform
	| fills |
	self edgeTransform: aTransform.
	self resetIfNeeded.
	fills := self registerFill: fillStyle and: borderFill.
	self primAddBezierShape: points
		segments: (points size) // 3
		fill: (fills at: 1)
		lineWidth: borderWidth
		lineFill: (fills at: 2).
	self postFlushIfNeeded.