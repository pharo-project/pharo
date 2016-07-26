pager-accessing
scrollBrick: aBrick

	scrollBrick := aBrick.
	self scrollBrick pagerModel: pagerModel.
	
	self scrollBrick
		vSpaceFill;
		hSpaceFill.
	
	self addBrickBack: self scrollBrick