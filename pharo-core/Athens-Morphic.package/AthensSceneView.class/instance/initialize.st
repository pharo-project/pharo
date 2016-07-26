initialize-release
initialize

	super initialize.
	keepRefreshing := false.
	transform := AthensAffineTransform new.
	
	self hResizing: #spaceFill.
	self vResizing: #spaceFill.
	color := Color transparent.
	state := Dictionary new.
	self extent: self minimumExtent.
	
