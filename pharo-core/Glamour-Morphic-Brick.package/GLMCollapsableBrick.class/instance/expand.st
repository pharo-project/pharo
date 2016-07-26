actions
expand

	self contentBrick ifNil: [ ^ self ].

	self isExpanded
		ifFalse: [ self addBrickBack: self contentBrick ]