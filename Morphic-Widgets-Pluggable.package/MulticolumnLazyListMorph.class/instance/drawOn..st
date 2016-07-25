drawing
drawOn: aCanvas
	self getListSize = 0 ifTrue:[ ^self ].

	self setColumnWidthsFor: aCanvas.
	self adjustWidth.
	
	super drawOn: aCanvas