drawing support
fillOval: rect
	| centerX centerY nextY yBias xBias outer nextOuterX |
	rect area <= 0 ifTrue: [^ self].
	height := 1.
	yBias := rect height odd ifTrue: [0] ifFalse: [-1].
	xBias := rect width odd ifTrue: [1] ifFalse: [0].
	centerX := rect center x.
	centerY := rect center y.
	outer := EllipseMidpointTracer new on: rect.
	nextY := rect height // 2.
	[nextY > 0] whileTrue:[
		nextOuterX := outer stepInY.
		width := (nextOuterX bitShift: 1) + xBias.
		destX := centerX - nextOuterX.
		destY := centerY - nextY.
		self copyBits.
		destY := centerY + nextY + yBias.
		self copyBits.
		nextY := nextY - 1.
	].
	destY := centerY.
	height := 1 + yBias.
	width := rect width.
	destX := rect left.
	self copyBits.
