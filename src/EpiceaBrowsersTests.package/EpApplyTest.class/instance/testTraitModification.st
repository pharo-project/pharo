tests
testTraitModification
	<expectedFailure>

	| aTrait traitName anotherTrait |
	aTrait := classFactory newTrait.
	anotherTrait := classFactory newTrait.
	traitName := aTrait name.
	aTrait setTraitComposition: anotherTrait asTraitComposition.

	"That should trigger a EpTraitModification and do not trigger many method events."


	self setHeadAsInputEntry.

	self class environment removeClassNamed: traitName.
	
	self assert: inputEntry content class equals: EpTraitModification.
	self deny: (self class environment includesKey: traitName).
	self applyInputEntry.
	self assert: (self class environment includesKey: traitName).
	self assert: aTrait traitComposition isEmpty.
