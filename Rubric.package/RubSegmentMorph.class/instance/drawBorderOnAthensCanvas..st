drawing
drawBorderOnAthensCanvas: aCanvas 
	self
		drawClippedBorderOnAthensCanvas: aCanvas
		usingEnds: (Array with: vertices first with: vertices last)