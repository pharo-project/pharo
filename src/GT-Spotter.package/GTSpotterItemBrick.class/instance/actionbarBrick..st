accessing
actionbarBrick: aBrick

	actionbarBrick ifNotNil: [ self removeBrick: actionbarBrick ].
	actionbarBrick := aBrick.

	
	self addBrickBack: actionbarBrick