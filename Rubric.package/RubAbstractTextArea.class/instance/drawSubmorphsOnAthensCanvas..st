drawing
drawSubmorphsOnAthensCanvas: aCanvas
	"Draw the focus here since we are using inset bounds
	for the focus rectangle."

	super drawSubmorphsOnAthensCanvas: aCanvas.
	self paragraph drawOnAthensCanvas: aCanvas bounds: self drawingBounds color: self textColor.
	(scrollPane isNil and: [ self readOnly not and: [ self hasKeyboardFocus or: [ self hasFindReplaceFocus ] ] ])
		ifTrue: [  self drawKeyboardFocusOnAthensCanvas: aCanvas ]