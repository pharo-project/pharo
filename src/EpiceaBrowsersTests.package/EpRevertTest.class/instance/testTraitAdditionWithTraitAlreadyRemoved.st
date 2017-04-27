tests
testTraitAdditionWithTraitAlreadyRemoved

	| aTrait traitName |
	aTrait := classFactory newTrait.
	traitName := aTrait name.
	self setHeadAsInputEntry.
	
	aTrait removeFromSystem.

	self deny: (self class environment includesKey: traitName).
	self revertInputEntry.
	self deny: (self class environment includesKey: traitName).
