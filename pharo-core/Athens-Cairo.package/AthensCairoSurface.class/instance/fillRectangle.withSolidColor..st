rendering dispatch
fillRectangle: aRectangle withSolidColor: aColor

	self loadSolidColor: aColor.
	
	currentCanvas
		newPath;
		rectangleX: aRectangle left y: aRectangle top width: aRectangle width height: aRectangle height;
		fill
	