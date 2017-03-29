rendering
renderBorderOf: aBrick on: aCanvas
	| markColor markWidth |
	aBrick border renderBorderOf: aBrick on: aCanvas.
	
	markColor := aBrick valueOfProperty: #brickMarkBorderColor ifAbsent: nil.
	markWidth := aBrick valueOfProperty: #brickMarkBorderWidth ifAbsent: nil.
	
	markColor isNotNil & markWidth isNotNil ifTrue: [
		aCanvas frameRectangle: aBrick brickBounds width: markWidth color: markColor ]