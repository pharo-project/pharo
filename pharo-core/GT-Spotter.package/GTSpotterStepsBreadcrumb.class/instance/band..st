accessing
band: aBrick

	band := aBrick.
	band
		useHorizontalLinearLayout;
		hShrinkWrap;
		vSpaceFill.
	
	self band data: 0.
	self addBrickBack: band.