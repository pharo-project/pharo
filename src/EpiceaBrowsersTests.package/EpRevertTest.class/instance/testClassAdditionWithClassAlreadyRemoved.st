tests
testClassAdditionWithClassAlreadyRemoved

	| aClass className |
	aClass := classFactory newClass.
	className := aClass name.
	self setHeadAsInputEntry.
	
	aClass removeFromSystem.
	
	self deny: (self class environment hasClassNamed: className).
	self revertInputEntry.
	self deny: (self class environment hasClassNamed: className).
