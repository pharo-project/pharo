accessing
textBrick: aBrick

	textBrick := aBrick.
	self textBrick
		wrapped: true.
		
	self textBrick layoutProperties
		vSpaceFill;
		hSpaceFill.
	self addBrickBack: self textBrick
	
