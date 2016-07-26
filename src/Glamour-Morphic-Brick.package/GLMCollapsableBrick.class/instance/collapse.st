actions
collapse

	self contentBrick ifNil: [ ^ self ].

	self isExpanded
		ifTrue: [ self removeBrick: self contentBrick ]