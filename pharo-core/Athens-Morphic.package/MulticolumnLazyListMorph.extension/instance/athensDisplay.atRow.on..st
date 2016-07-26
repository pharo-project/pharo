*Athens-Morphic
athensDisplay: items atRow: row on: anAthensCanvas
	| drawBounds backgroundColor |
	backgroundColor := self backgroundColorForRow: row.
	drawBounds := self drawBoundsForRow: row.
	drawBounds := drawBounds
		intersect: self bounds
		ifNone: [ 
			"oh well"
			^ self ].
	items
		with: (1 to: items size)
		do: [ :item :index | 
			"move the bounds to the right at each step"
			index > 1
				ifTrue: [ drawBounds := drawBounds left: drawBounds left + listSource gapSize + (columnWidths at: index - 1) ].
			anAthensCanvas
				morphicDrawString: item asString
				at: drawBounds topLeft
				font: self font
				color: color]