tests
testClassAdditionWithClassRemoved

	| aClass className |
	aClass := classFactory newClass.
	className := aClass name.
	self setHeadAsInputEntry.
	
	aClass removeFromSystem.
	
	self deny: (self class environment hasClassNamed: className).
	self applyInputEntry.
	self assert: (self class environment hasClassNamed: className).
