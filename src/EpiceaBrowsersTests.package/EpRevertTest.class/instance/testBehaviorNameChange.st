tests
testBehaviorNameChange

	| aClass className |
	aClass := classFactory newClass.
	className := aClass name.
	aClass rename: className, '2'.
	self setHeadAsInputEntry.

	self revertInputEntry.
	
	self assert: aClass name equals: className.
