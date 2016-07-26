canvas drawing-rectangles
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth topLeftColor: topLeftColor bottomRightColor: bottomRightColor
	self fillRectangle: r fillStyle: fillColor.
	topLeftColor = bottomRightColor
		ifTrue: [ self frameRectangle: r width: borderWidth color: bottomRightColor ]