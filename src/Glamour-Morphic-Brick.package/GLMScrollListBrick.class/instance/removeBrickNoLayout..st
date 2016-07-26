adding/removing
removeBrickNoLayout: aBrick

	self band doNotLayoutDuring: [ self band removeBrick: aBrick ]