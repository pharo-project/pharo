tests
testClassAddition

	| aClass className |
	aClass := classFactory newClass.
	className := aClass name.
	self setHeadAsInputEntry.
	
	self assert: (self class environment hasClassNamed: className).
	self revertInputEntry.
	self deny: (self class environment hasClassNamed: className).
