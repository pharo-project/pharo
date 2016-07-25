adding/removing
addBrickBackNoLayout: aBrick

	self band doNotLayoutDuring: [ self band addBrickBack: aBrick ].
	^ aBrick