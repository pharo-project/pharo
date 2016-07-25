brick-structure-adding/removing
removeBrick: aBrick
	|result|
	aBrick ifNil: [ ^ aBrick ].
	result := self privateRemoveBrick: aBrick.
	self	privatePrepareBrickAfterRemoving: aBrick.
	self doLayoutForce.
	^ result