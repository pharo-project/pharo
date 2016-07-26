accessing
nameBrick: aBrick

	nameBrick ifNotNil: [self removeBrick: nameBrick].
	nameBrick := aBrick.
	
	self themer spotterThemer categoryTitleNameStyleFor: nameBrick.
	
	self addBrickBack: nameBrick