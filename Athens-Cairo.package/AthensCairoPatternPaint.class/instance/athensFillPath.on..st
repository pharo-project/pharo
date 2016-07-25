drawing
athensFillPath: path on: aCanvas 

	aCanvas
		newPath;
		loadPath: path.
	self setMatrix: aCanvas paintTransform inverted.
	self loadOnCairoCanvas: aCanvas.
	
	aCanvas fill.