initialization
initialize
	super initialize.
	self 
		color: Color transparent;
		borderWidth: 0;
		vResizing: #spaceFill;
		hResizing: #spaceFill.
	self 
		initializeTransform;
		initializeScrollbar.
	paneCount := 0.
	maxPanes := 2.