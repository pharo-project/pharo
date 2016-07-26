brick-structure-adding/removing
addBrick: aBrick after: otherBrick
	|result|
	result := self privateAddBrick: aBrick asBrick after: otherBrick.
	self privatePrepareBrickAfterAdding: result.
	self doLayoutForce.
	^ aBrick