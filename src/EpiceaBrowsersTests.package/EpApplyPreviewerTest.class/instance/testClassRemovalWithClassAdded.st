tests
testClassRemovalWithClassAdded

	| aClass className classDefinition |
	aClass := classFactory newClass.
	className := aClass name.
	classDefinition := aClass definition.
	aClass removeFromSystem.
	
	self setHeadAsInputEntry.
	
	aClass := classFactory newClass.
	aClass rename: className.
	
	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpClassRemoval.
		self assert: output behaviorAffected definitionSource equals: aClass definition.
		]
