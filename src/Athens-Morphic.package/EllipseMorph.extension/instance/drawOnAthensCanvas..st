*Athens-Morphic
drawOnAthensCanvas: anAthensCanvas
	| border ellipse |
	self bounds area > 0
		ifFalse: [ ^ self ].
	border := self borderStyle.
	ellipse := self asAthensShapeOn: anAthensCanvas.
	anAthensCanvas setPaint: self fillStyle.
	anAthensCanvas setShape: ellipse.
	anAthensCanvas draw.
	(anAthensCanvas setStrokePaint: border color) width: border width.
	anAthensCanvas draw