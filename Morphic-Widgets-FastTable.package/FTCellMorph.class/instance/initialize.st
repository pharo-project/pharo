initialization
initialize
	super initialize.
	topSeparator := false.
	self
		clipSubmorphs: true;
		layoutPolicy: TableLayout new;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		listDirection: #leftToRight;
		listCentering: #topLeft;
		wrapCentering: #center