tests
testClassAdditionWithClassAlreadyRemoved

	| aClass anEntry className |
	aClass := classFactory newClass.
	className := aClass name.
	anEntry := monitor log head.
	
	aClass removeFromSystem.
	
	self deny: (self class environment hasClassNamed: className).
	self revert: { anEntry }.
	self deny: (self class environment hasClassNamed: className).
