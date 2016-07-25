adding
addAColumn: aCollectionOfMorphs

	| col |
	col := self class inAColumn: aCollectionOfMorphs.
	self addMorphBack: col.
	^col