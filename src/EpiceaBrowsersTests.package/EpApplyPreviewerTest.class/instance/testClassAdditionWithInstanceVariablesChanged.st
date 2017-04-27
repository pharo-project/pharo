tests
testClassAdditionWithInstanceVariablesChanged

	| aClass classDefinition |
	aClass := classFactory newClass.
	classDefinition := aClass definition.
	self setHeadAsInputEntry.
	aClass addInstVarNamed: #x.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpClassModification.
		self assert: output oldClass definitionSource equals: aClass definition.
		self assert: output newClass definitionSource equals: classDefinition.
		]
