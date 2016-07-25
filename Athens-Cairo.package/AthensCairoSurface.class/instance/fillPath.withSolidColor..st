rendering dispatch
fillPath: aPath withSolidColor: aColor 

	self loadSolidColor: aColor.
	
	currentCanvas 
		newPath;
		loadPath: aPath;
		fill
	