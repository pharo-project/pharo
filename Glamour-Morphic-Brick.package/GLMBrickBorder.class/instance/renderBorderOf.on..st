rendering
renderBorderOf: aBrick on: aCanvas
	|canvas widthLeft widthRight widthTop widthBottom colorLeft colorRight colorTop colorBottom|
	
	canvas := aCanvas.
	
	widthLeft := self widthLeft brickValue: aBrick.
	widthRight := self widthRight brickValue: aBrick.
	widthTop := self widthTop brickValue: aBrick.
	widthBottom := self widthBottom brickValue: aBrick.
	
	colorLeft := self colorLeft brickValue: aBrick.
	colorRight := self colorRight brickValue: aBrick.
	colorTop := self colorTop brickValue: aBrick.
	colorBottom := self colorBottom brickValue: aBrick.
	
	canvas
		fillRectangle: (aBrick brickBounds origin extent: ((aBrick brickBounds width - widthRight) @ widthTop))
		color: colorTop.
		
	canvas
		fillRectangle: ((aBrick brickBounds right - widthRight) @ aBrick brickBounds top corner: (aBrick brickBounds corner - (0@widthBottom)))
		color: colorRight.
		
	canvas
		fillRectangle: ((aBrick brickBounds bottomLeft - (widthLeft negated @ widthBottom)) corner: aBrick brickBounds corner)
		color: colorBottom.
		
	canvas
		fillRectangle: ((aBrick brickBounds origin + (0@widthTop)) corner: (aBrick brickBounds bottomLeft + (widthLeft@0)))
		color: colorLeft.