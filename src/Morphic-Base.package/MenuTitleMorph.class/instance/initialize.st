initialization
initialize
	super initialize.
	
	self 
	listDirection: #leftToRight;
	vResizing: #shrinkWrap;
	wrapCentering: #topLeft;
	cellPositioning: #center;
	cellSpacing: #none;
	layoutInset: 2;
	clipSubmorphs: true