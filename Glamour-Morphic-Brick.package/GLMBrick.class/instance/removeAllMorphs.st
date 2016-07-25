morph-common
removeAllMorphs
	| oldMorphs myWorld |
	"we don't invalidate world"
	myWorld := self world.
	submorphs do: [:m | myWorld ifNotNil: [ m outOfWorld: myWorld ]. m privateOwner: nil].
	oldMorphs := submorphs.
	submorphs := EmptyArray.
	oldMorphs do: [ :m | self removedMorph: m ].
