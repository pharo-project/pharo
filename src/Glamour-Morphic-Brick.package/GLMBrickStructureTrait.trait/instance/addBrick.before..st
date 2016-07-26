brick-structure-adding/removing
addBrick: aBrick before: otherBrick
	|result|
	result := self privateAddBrick: aBrick asBrick before: otherBrick.
	self privatePrepareBrickAfterAdding: result.
	self doLayoutForce.
	^ aBrick