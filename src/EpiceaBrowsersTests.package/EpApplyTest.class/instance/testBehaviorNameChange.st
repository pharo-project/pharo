tests
testBehaviorNameChange

	| aClass oldName newName |
	aClass := classFactory newClass.
	oldName := aClass name.
	newName := oldName, '2'.
	aClass rename: newName.
	self setHeadAsInputEntry.

	aClass rename: oldName. "Rollback"

	self assert: aClass name equals: oldName.
	self applyInputEntry.
	self assert: aClass name equals: newName.