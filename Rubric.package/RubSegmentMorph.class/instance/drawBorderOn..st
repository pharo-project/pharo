drawing
drawBorderOn: aCanvas 
	self
		drawClippedBorderOn: aCanvas
		usingEnds: (Array with: vertices first with: vertices last)