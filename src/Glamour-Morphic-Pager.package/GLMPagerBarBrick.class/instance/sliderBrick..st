bar-accessing
sliderBrick: aBrick

	sliderBrick := aBrick.
	
	self sliderBrick
		pagerModel: self pagerModel;
		height: self pagerModel circleButtonSize;
		width: [ self pagerModel circleButtonSize * self pagerModel actualNumberOfVisiblePages ].
		
	self sliderBrick layout
		floating: #left.
		
	self addBrickBack: self sliderBrick