rendering
renderBackgroundColorOf: aBrick on: aCanvas
	|rectangle|
	rectangle := aBrick brickBounds expandBy: aBrick shadowWidth negated.
	aCanvas fillRectangle: rectangle color: aBrick color.
	(aBrick fillStyle = Color transparent) ifFalse: [ 
		aCanvas fillRectangle: rectangle fillStyle: aBrick fillStyle borderStyle: aBrick borderStyle ]