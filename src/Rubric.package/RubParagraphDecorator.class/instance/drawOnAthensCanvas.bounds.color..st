drawing
drawOnAthensCanvas: aCanvas bounds: aRectangle color: aColor

   (self canDrawDecoratorsOn: aCanvas) ifFalse: [ ^self ].

	self drawOnAthensCanvas: aCanvas.
	next drawOnAthensCanvas: aCanvas bounds: aRectangle color: aColor