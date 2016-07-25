submorphs-add/remove
removeAllMorphs
	| oldMorphs myWorld |
	myWorld := self world.
	(fullBounds notNil or:[myWorld notNil]) ifTrue:[self invalidRect: self fullBounds].
	submorphs do: [:m | myWorld ifNotNil: [ m outOfWorld: myWorld ]. m privateOwner: nil].
	oldMorphs := submorphs.
	submorphs := EmptyArray.
	oldMorphs do: [ :m | self removedMorph: m ].
	self layoutChanged.
