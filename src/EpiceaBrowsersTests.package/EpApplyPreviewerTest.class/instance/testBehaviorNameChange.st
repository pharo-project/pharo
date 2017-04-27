tests
testBehaviorNameChange

	| aClass oldName newName |
	aClass := classFactory newClass.
	oldName := aClass name.
	newName := oldName, '2'.
	aClass rename: newName.
	self setHeadAsInputEntry.

	aClass rename: oldName. "Rollback"

	self assertOutputsAnEventWith: [:output |
		self assert: output class equals: EpBehaviorNameChange.
		self assert: output oldName equals: oldName.
		self assert: output newName equals: newName.
		]
