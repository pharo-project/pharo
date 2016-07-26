accessing
contentsBrick: aBrick
	
	contentsBrick ifNotNil: [self removeBrick: contentsBrick].
	contentsBrick := aBrick.
		
	self themer spotterThemer spotterContentsStyleFor: contentsBrick.

	self addBrickBack: contentsBrick