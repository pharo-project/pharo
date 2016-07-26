rendering
renderColorOf: aBrick on: aCanvas
	|color|
	color := aBrick currentColor.
	color ifNotNil: [
		aCanvas fillRectangle: (aBrick brickBounds expandBy: aBrick shadowWidth negated) color: color ]