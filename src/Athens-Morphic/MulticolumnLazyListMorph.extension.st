Extension { #name : #MulticolumnLazyListMorph }

{ #category : #'*Athens-Morphic' }
MulticolumnLazyListMorph >> athensDisplay: items atRow: row on: anAthensCanvas [
	| drawBounds |
	drawBounds := self drawBoundsForRow: row.
	drawBounds := drawBounds
		              intersect: self bounds
		              ifNone: [ "oh well" ^ self ].
	items
		with: (1 to: items size)
		do: [ :item :index | "move the bounds to the right at each step"
			index > 1 ifTrue: [
				drawBounds := drawBounds left:
					              drawBounds left + listSource gapSize
					              + (columnWidths at: index - 1) ].
			anAthensCanvas
				morphicDrawString: item asString
				at: drawBounds topLeft
				font: self font
				color: color ]
]

{ #category : #'*Athens-Morphic' }
MulticolumnLazyListMorph >> drawOnAthensCanvas: anAthensCanvas [
	self getListSize = 0 ifTrue:[ ^self ].

	self setColumnWidthsFor: anAthensCanvas.
	self adjustWidth.

	super drawOnAthensCanvas: anAthensCanvas
]
