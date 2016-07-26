drawing
athensFillPath: aPath on: aCanvas
	
	self prepareForDrawingOn: aCanvas.
	
	aCanvas 
		newPath; 
		loadPath: aPath;
		stroke