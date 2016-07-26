drawing
drawOn: aCanvas
	"Draw the selection and lines."
	
	super drawOn: aCanvas.
	searchedElement ifNotNil:
		[aCanvas clipBy: self innerBounds during: [:c |
			c
				fillRectangle: self searchedFrame
				color: (self secondarySelectionColor ifNil: [self theme secondarySelectionColor])]].
	selectedMorph ifNotNil:
		[aCanvas clipBy: self innerBounds during: [:c |
			c
				fillRectangle: self selectionFrame
				color: (self selectionColorToUse ifNil: [ self theme selectionColor ]) ] ].
	self drawLinesOn: aCanvas