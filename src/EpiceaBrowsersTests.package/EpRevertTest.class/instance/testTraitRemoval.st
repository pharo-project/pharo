tests
testTraitRemoval

	| aTrait traitName |
	aTrait := classFactory newTrait.
	traitName := aTrait name.
	aTrait removeFromSystem.
	self setHeadAsInputEntry.

	self deny: (self class environment includesKey: traitName).
	self revertInputEntry.
	self assert: (self class environment includesKey: traitName).
	"---> Fails to create trait with Unclassified category"