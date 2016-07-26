drawing
drawMouseDownHighlightOn: aCanvas
	self highlightedForMouseDown ifTrue: [
		aCanvas frameRectangle: self fullBounds color: self color darker darker].