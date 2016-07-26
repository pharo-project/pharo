actions
moveTo: anItemBrick
	|itemBounds delta selfBounds|
	anItemBrick ifNil: [ ^ self ].
	
	itemBounds := anItemBrick globalBounds.
	selfBounds := self globalBounds.
	
	delta := ((itemBounds top - selfBounds top) max: 0) min: (selfBounds height - self triangleBrick height).
	self triangleBrick brickBounds top = delta ifTrue: [ ^ self ].
	
	self triangleBrick marginTop: delta.
	self triangleBrick doLayoutForce.
	self owner changed
	