accessing
amountBrick: aBrick

	amountBrick ifNotNil: [self removeBrick: amountBrick].
	amountBrick := aBrick.
	
	self themer spotterThemer categoryAmountStyleFor: amountBrick.
	
	self addBrickBack: amountBrick