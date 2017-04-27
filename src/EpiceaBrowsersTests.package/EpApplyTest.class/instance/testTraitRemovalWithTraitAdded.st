tests
testTraitRemovalWithTraitAdded

	| aTrait traitName |
	aTrait := classFactory newTrait.
	traitName := aTrait name.
	aTrait removeFromSystem.
	self setHeadAsInputEntry.

	aTrait := classFactory newTrait.
	aTrait rename: traitName.

	self assert: (inputEntry content isKindOf: EpTraitRemoval).
	self assert: (self class environment includesKey: traitName).
	self applyInputEntry.
	self deny: (self class environment includesKey: traitName).
