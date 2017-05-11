tests
testTraitRemovalWithTraitAdded

	| aTrait traitName traitDefinition |
	aTrait := classFactory newTrait.
	traitName := aTrait name.
	traitDefinition := aTrait definition.
	aTrait removeFromSystem.
	self setHeadAsInputEntry.

	aTrait := classFactory newTrait.
	aTrait rename: traitName.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpTraitRemoval.
		self assert: output behaviorAffected definitionSource equals: traitDefinition.
		]
