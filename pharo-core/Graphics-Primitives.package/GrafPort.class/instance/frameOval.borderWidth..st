drawing support
frameOval: rect borderWidth: borderWidth
	| centerX centerY nextY yBias xBias wp outer inner nextOuterX nextInnerX |
	rect area <= 0 ifTrue: [^ self].
	height := 1.
	wp := borderWidth asPoint.
	yBias := rect height odd ifTrue: [0] ifFalse: [-1].
	xBias := rect width odd ifTrue: [1] ifFalse: [0].
	centerX := rect center x.
	centerY := rect center y.
	outer := EllipseMidpointTracer new on: rect.
	inner := EllipseMidpointTracer new on: (rect insetBy: wp).
	nextY := rect height // 2.
	1 to: (wp y min: nextY) do:[:i|
		nextOuterX := outer stepInY.
		width := (nextOuterX bitShift: 1) + xBias.
		destX := centerX - nextOuterX.
		destY := centerY - nextY.
		self copyBits.
		destY := centerY + nextY + yBias.
		self copyBits.
		nextY := nextY - 1.
	].
	[nextY > 0] whileTrue:[
		nextOuterX := outer stepInY.
		nextInnerX := inner stepInY.
		destX := centerX - nextOuterX.
		destY := centerY - nextY.
		width := nextOuterX - nextInnerX.
		self copyBits.
		destX := centerX + nextInnerX + xBias.
		self copyBits.
		destX := centerX - nextOuterX.
		destY := centerY + nextY + yBias.
		self copyBits.
		destX := centerX + nextInnerX + xBias.
		self copyBits.
		nextY := nextY - 1.
	].
	destY := centerY.
	height := 1 + yBias.
	width := wp x.
	destX := rect left.
	self copyBits.
	destX := rect right - wp x.
	self copyBits.
