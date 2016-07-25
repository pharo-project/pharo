drawing
drawOnAthensCanvas: aCanvas
	super drawOnAthensCanvas: aCanvas.

	self drawSelectionOn: aCanvas.
	

	self hasKeyboardFocus ifTrue: [ 
			self drawKeyboardFocusOn: aCanvas.
		]