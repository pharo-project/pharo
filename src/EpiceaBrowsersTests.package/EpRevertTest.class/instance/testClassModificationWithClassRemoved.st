tests
testClassModificationWithClassRemoved

	| aClass anEntry className |
	aClass := classFactory newClass.
	className := aClass name.
	aClass addInstVarNamed: #x.
	anEntry := monitor log head.
	
	aClass removeFromSystem.
	
	self assert: (anEntry content isKindOf: EpClassModification).
	self deny: (self class environment hasClassNamed: className).
	self revert: { anEntry }.
	self 
		assert: (self class environment hasClassNamed: className)
		description: 'Revert of a modification of a class that was removed will add it back with the original look'.
	self
		assert: (self class environment classNamed: className) instVarNames isEmpty
		description: 'The old class of the modification had no instance variables'.
