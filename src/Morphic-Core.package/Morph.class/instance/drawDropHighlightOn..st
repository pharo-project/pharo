drawing
drawDropHighlightOn: aCanvas
	self highlightedForDrop ifTrue: [
		aCanvas frameRectangle: self fullBounds color: self dropHighlightColor].