*Athens-Morphic
displaySelectionBarOnAthensCanvas: anAthensCanvas
	| visibleRectangle line |
	selectionStart ifNil: [ ^ self ].
	selectionStop ifNil: [ ^ self ].
	visibleRectangle := anAthensCanvas clipRect.
	selectionStart textLine = selectionStop textLine
		ifFalse: [ ^ self ].
	line := selectionStart textLine.
	anAthensCanvas setPaint: self selectionBarColor.
	anAthensCanvas drawShape: (visibleRectangle left @ line top corner: visibleRectangle right @ line bottom)