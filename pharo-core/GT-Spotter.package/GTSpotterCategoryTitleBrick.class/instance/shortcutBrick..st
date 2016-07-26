accessing
shortcutBrick: aBrick

	shortcutBrick ifNotNil: [self removeBrick: shortcutBrick].
	shortcutBrick := aBrick.
	
	self themer spotterThemer categoryTitleShortcutStyleFor: shortcutBrick.
	
	self addBrickBack: shortcutBrick