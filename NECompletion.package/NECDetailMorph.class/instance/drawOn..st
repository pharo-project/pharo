drawing
drawOn: aCanvas 
	super drawOn: aCanvas.
	arrowPosition ifNotNil: [
		self drawArrowOn: aCanvas.
		self drawMessageOn: aCanvas]