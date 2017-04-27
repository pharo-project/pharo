tests
testClassAdditionWithSuperclassChanged

	| aClass classDefinition |
	aClass := classFactory newClass.
	classDefinition := aClass definition.
	self setHeadAsInputEntry.
	aClass superclass: Array.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpClassModification.
		self assert: output oldClass definitionSource equals: aClass definition.
		self assert: output newClass definitionSource equals: classDefinition.
		].

	aClass superclass: Object. "Restore original superclass else something in hierarchy stays wrong (and ClassHierarchyTest>>testSubclassInstVar fails)"
