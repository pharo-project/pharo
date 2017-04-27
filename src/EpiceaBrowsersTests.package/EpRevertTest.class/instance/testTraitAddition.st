tests
testTraitAddition

	| aTrait traitName |
	aTrait := classFactory newTrait.
	traitName := aTrait name.
	self setHeadAsInputEntry.

	self assert: (self class environment includesKey: traitName).
	self revertInputEntry.
	self deny: (self class environment includesKey: traitName).
