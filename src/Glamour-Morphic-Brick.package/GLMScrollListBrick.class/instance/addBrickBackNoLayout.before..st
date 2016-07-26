adding/removing
addBrickBackNoLayout: aBrick before: anotherBrick

	self band doNotLayoutDuring: [ self band addBrick: aBrick before: anotherBrick ].
	^ aBrick