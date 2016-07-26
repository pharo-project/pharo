canvas drawing-rectangles
fillRectangle: aRectangle color: aColor
	self canvas setPaint: aColor.
	self canvas drawShape: aRectangle