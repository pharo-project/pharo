drawing
highlightPotentialDropRow: row  on: aCanvas
	| drawBounds  |
	drawBounds := self drawBoundsForRow: row.
	drawBounds := drawBounds intersect: self bounds ifNone: [ ^ self ].
	aCanvas frameRectangle: drawBounds color: Color blue