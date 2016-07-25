adding
addARow: aCollectionOfMorphs

	| row |
	row := self class inARow: aCollectionOfMorphs.
	self addMorphBack: row.
	^row