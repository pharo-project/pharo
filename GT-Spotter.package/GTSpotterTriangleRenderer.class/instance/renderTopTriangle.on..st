rendering
renderTopTriangle: aBrick on: aCanvas
	|a b c width height|
	
	width := aBrick brickBounds width.
	height := aBrick brickBounds height.
	
	a := aBrick brickBounds bottomLeft + (((width / 2 - height) max: 0) @ 0).
	b := aBrick brickBounds bottomLeft + (((width / 2 + height) min: width) @ 0).
	c := aBrick brickBounds topLeft + (width / 2 @ 0).
	
	self renderTriangle: aBrick on: aCanvas with: { a asIntegerPoint . b asIntegerPoint . c asIntegerPoint}