bar-accessing
buttonsBrick: aBrick

	buttonsBrick := aBrick.
	
	self buttonsBrick pagerModel: pagerModel.
	
	self buttonsBrick
		hShrinkWrap;
		vShrinkWrap.
	
	self addBrickBack: self buttonsBrick