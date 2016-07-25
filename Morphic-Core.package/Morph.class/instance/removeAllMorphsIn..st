submorphs-add/remove
removeAllMorphsIn: aCollection
	"greatly speeds up the removal of *lots* of submorphs"
	| set myWorld |
	set := IdentitySet new: aCollection size * 4 // 3.
	aCollection do: [:each | each owner == self ifTrue: [ set add: each]].
	myWorld := self world.
	(fullBounds notNil or:[myWorld notNil]) ifTrue:[self invalidRect: self fullBounds].
	set do: [:m | myWorld ifNotNil: [ m outOfWorld: myWorld ]. m privateOwner: nil].
	submorphs := submorphs reject: [ :each | set includes: each].
	set do: [ :m | self removedMorph: m ].
	self layoutChanged.
