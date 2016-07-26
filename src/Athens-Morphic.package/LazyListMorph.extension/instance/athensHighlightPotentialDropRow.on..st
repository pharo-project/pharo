*Athens-Morphic
athensHighlightPotentialDropRow: row on: anAthensCanvas
	| drawBounds |
	drawBounds := self drawBoundsForRow: row.
	drawBounds := drawBounds intersect: self bounds ifNone: [ ^ self ].
	anAthensCanvas setStrokePaint: Color blue.
	anAthensCanvas drawShape: drawBounds