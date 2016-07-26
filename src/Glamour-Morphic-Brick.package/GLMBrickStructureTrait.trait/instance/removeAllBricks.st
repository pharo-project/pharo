brick-structure-adding/removing
removeAllBricks
	|copy|
	copy := self subbricks copy.
	self privateRemoveAllBricks.
	copy do: [ :b | self privatePrepareBrickAfterRemoving: b ].
	self doLayoutForce