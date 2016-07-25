drawing
athensFillRectangle: aRect on: aCanvas

	self prepareForDrawingOn: aCanvas.
	
	aCanvas 
		newPath;
		rectangleX: aRect left
		y: aRect top 
		width: aRect width
		height: aRect height;
		stroke.
