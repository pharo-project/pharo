accessing
textBrick: aBrick

	textBrick := aBrick.
	
	self textBrick
		margin: #(0 3 3 3);
		vShrinkWrap.
	
	self addBrickBack: self textBrick