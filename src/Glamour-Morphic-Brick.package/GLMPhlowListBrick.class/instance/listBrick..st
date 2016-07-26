accessing
listBrick: aBrick

	scrollList := aBrick.
	self listBrick
		vSpaceFill;
		hSpaceFill.
		
	self addBrickBack: self listBrick