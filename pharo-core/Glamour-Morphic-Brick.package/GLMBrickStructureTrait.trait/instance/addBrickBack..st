brick-structure-adding/removing
addBrickBack: aBrick
	|result|
	result := self privateAddBrickBack: aBrick asBrick.
	self privatePrepareBrickAfterAdding: result.
	self doLayoutForce.
	^ aBrick