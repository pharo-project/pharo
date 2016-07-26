*Athens-Morphic
drawOnAthensCanvas: anAthensCanvas
	| border shape stroke |
	border := self borderStyle.
	shape := self asAthensPathOn: anAthensCanvas.
	self isClosed
		ifTrue: [ 
			anAthensCanvas setPaint: self fillStyle.
			anAthensCanvas drawShape: shape ].
	stroke := anAthensCanvas setStrokePaint: border color.
	stroke width: border width.
	self dashedBorder notNil
		ifTrue: [ self setDashOnStroke: stroke ].
	anAthensCanvas drawShape: shape.
	self hasTwoColorDash
		ifTrue: [ 
			self setSecondColorDashOnStroke: stroke.
			anAthensCanvas drawShape: shape ].
	self drawArrowsOnAthensCanvas: anAthensCanvas