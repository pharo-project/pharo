brick-structure-adding/removing
addBrickFirst: aBrick
	|result|
	result := self privateAddBrickFirst: aBrick asBrick.
	self privatePrepareBrickAfterAdding: result.
	self doLayoutForce.
	^ aBrick