adding/removing
addItemMorph: aMorph after: anotherMorph

	self addBrickBackNoLayout: aMorph.
	self setBoundsOf: aMorph after: anotherMorph.
	(self isPartiallyVisible: aMorph) ifFalse: [ self removeBrickNoLayout: aMorph. ^ nil ].
	^ aMorph