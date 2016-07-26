canvas drawing-rectangles
frameRectangle: aRectangle width: aBorderWidth color: aColor
	(self canvas setStrokePaint: aColor) width: aBorderWidth.
	self canvas drawShape: aRectangle