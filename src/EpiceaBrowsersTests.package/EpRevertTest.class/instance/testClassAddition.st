tests
testClassAddition

	| aClass anEntry className |
	aClass := classFactory newClass.
	className := aClass name.
	anEntry := monitor log head.
	
	self assert: (self class environment hasClassNamed: className).
	self revert: { anEntry }.
	self deny: (self class environment hasClassNamed: className).
