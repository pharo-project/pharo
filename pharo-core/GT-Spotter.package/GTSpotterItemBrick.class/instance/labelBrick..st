accessing
labelBrick: aBrick

	labelBrick ifNotNil: [self removeBrick: labelBrick].
	labelBrick := aBrick.
	
	self themer spotterThemer itemLabelStyleFor: labelBrick.
	
	self addBrickBack: labelBrick