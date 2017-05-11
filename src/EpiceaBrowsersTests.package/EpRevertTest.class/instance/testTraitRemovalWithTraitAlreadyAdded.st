tests
testTraitRemovalWithTraitAlreadyAdded

	| aTrait traitName |
	aTrait := classFactory newTrait.
	traitName := aTrait name.
	aTrait removeFromSystem.
	self setHeadAsInputEntry.

	"Restore trait"
	aTrait := classFactory newTrait.
	aTrait rename: traitName.

	self assert: (self class environment includesKey: traitName).
	self revertInputEntry.
	self assert: (self class environment includesKey: traitName).
