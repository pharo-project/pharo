accessing
iconBrick: aBrick

	iconBrick ifNotNil: [self removeBrick: iconBrick].
	iconBrick := aBrick.
	
	self themer spotterThemer itemIconStyleFor: iconBrick.
	
	self addBrickBack: iconBrick