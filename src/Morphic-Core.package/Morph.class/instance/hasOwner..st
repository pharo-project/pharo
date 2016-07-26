structure
hasOwner: aMorph
	"Return true if the receiver has aMorph in its owner chain"
	aMorph ifNil:[^true].
	self allOwnersDo:[:m| m = aMorph ifTrue:[^true]].
	^false