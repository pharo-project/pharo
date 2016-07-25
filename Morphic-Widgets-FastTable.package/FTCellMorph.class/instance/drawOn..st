drawing
drawOn: aCanvas
	super drawOn: aCanvas.
	topSeparator
		ifTrue: [ aCanvas line: bounds topLeft to: bounds topRight color: self seperatorColor ]