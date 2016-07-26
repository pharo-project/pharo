accessing
headerBrick: aBrick
	
	headerBrick ifNotNil: [self removeBrick: headerBrick].
	headerBrick := aBrick.
	
	self themer spotterThemer headerStyleFor: headerBrick.
	
	self addBrickBack: headerBrick