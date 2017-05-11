tests
testClassAdditionWithClassRemoved

	| aClass className classDefinition |
	aClass := classFactory newClass.
	className := aClass name.
	classDefinition := aClass definition.
	self setHeadAsInputEntry.

	aClass removeFromSystem.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpClassAddition.
		self assert: output behaviorAffected definitionSource equals: classDefinition.
		]