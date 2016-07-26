accessing
actionbarBrick: aBrick

	actionbarBrick ifNotNil: [self removeBrick: actionbarBrick].
	actionbarBrick := aBrick.
	
	self themer spotterThemer itemActionbarStyleFor: aBrick.
	
	self addBrickBack: actionbarBrick