initialization
initialize

	super initialize.

	self 	
		changeTableLayout;
		listDirection: #leftToRight;
		cellPositioning: #center;
		cellInset: 2;
		borderWidth: 0;
		color: Color transparent;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		extent: 20@16.