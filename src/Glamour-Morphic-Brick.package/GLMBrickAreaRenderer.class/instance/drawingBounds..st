geometry
drawingBounds: aBrick
	| aRectangle anOffset |
	
	anOffset := aBrick shadowOffset.
	
	aRectangle := aBrick brickBounds expandBy: (aBrick border widthTop + aBrick shadowWidth).
	anOffset isZero
		ifFalse: [ aRectangle := aRectangle quickMerge: (aRectangle translateBy: anOffset) ].
	
	^ aRectangle