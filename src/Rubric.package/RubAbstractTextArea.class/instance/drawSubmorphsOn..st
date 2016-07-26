drawing
drawSubmorphsOn: aCanvas
	"Draw the focus here since we are using inset bounds
	for the focus rectangle."

	super drawSubmorphsOn: aCanvas.
	aCanvas rubParagraph: self paragraph bounds: self drawingBounds color: self textColor.
	(scrollPane isNil and: [ self readOnly not and: [ self hasKeyboardFocus or: [ self hasFindReplaceFocus ] ] ])
		ifTrue: [self drawKeyboardFocusOn: aCanvas ]