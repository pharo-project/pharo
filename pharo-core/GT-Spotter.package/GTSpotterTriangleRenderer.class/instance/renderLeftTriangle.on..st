rendering
renderLeftTriangle: aBrick on: aCanvas
	|a b c width height|
	
	width := aBrick brickBounds width.
	height := aBrick brickBounds height.
	
	a := aBrick brickBounds topRight + (0 @ ((height / 2 - width) max: 0)).
	b := aBrick brickBounds topRight + (0 @ ((height / 2 + width) min: height)).
	c := aBrick brickBounds topLeft + (0 @ (height / 2)).
	
	self renderTriangle: aBrick on: aCanvas with: { a asIntegerPoint . b asIntegerPoint . c asIntegerPoint}