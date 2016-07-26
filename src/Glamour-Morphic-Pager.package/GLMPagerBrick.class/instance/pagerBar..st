pager-accessing
pagerBar: aBrick

	pagerBar := aBrick.
	
	self pagerBar
		vShrinkWrap;
		hShrinkWrap.
	self pagerBar layout hAlign: #center.
	
	self pagerBar pagerModel: pagerModel.
	
	self addBrickBack: self pagerBar