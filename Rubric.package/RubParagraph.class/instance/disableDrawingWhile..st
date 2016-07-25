drawing
disableDrawingWhile: aBlock
	self drawingEnabled ifFalse: [ ^ aBlock value ].
	self disableDrawing.
	aBlock
		ensure: [ self enableDrawing ]