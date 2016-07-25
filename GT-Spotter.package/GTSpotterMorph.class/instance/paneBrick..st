accessing
paneBrick: aBrick
	
	paneBrick ifNotNil: [self removeBrick: paneBrick].
	paneBrick := aBrick.
		
	self themer spotterThemer spotterPaneStyleFor: paneBrick.

	self addBrickBack: paneBrick