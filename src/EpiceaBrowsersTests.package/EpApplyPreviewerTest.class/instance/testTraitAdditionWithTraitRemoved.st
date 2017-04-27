tests
testTraitAdditionWithTraitRemoved

	| aTrait traitDefinition |
	aTrait := classFactory newTrait.
	traitDefinition := aTrait definition.
	self setHeadAsInputEntry.

	aTrait removeFromSystem.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpTraitAddition.
		self assert: output behaviorAffected definitionSource equals: traitDefinition.
		]