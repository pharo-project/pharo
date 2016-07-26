drawing
athensFillPath: path on: aCanvas 

	aCanvas
		setSourceR: r g: g b: b a: a; 
		newPath;
		loadPath: path;
		fill.
	
