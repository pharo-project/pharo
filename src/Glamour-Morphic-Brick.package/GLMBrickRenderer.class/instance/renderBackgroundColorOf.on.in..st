private
renderBackgroundColorOf: aBrick on: aCanvas in: aRectangle

	aCanvas fillRectangle: aRectangle color: aBrick color.
	(aBrick fillStyle = Color transparent) ifFalse: [ 
		aCanvas fillRectangle: aRectangle fillStyle: aBrick fillStyle borderStyle: aBrick borderStyle ]