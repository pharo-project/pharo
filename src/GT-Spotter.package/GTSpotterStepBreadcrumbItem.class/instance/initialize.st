initialization
initialize
	super initialize.
	
	self
		useVerticalLinearLayout;
		labelBrick: self newLabelBrick;
		triangleBrick: self newTriangleBrick;
		enable
	