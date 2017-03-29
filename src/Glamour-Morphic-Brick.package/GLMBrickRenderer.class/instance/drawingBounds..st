geometry
drawingBounds: aBrick
	| aRectangle anOffset |

	aRectangle := aBrick brickBounds.
	anOffset := aBrick shadowOffset.

	^ aBrick shadowWidth isZero
		ifTrue: [ aRectangle ]
		ifFalse: [ aRectangle quickMerge: ((aRectangle expandBy: aBrick shadowWidth) translateBy: anOffset) ]