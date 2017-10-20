tests - working
testContraintShouldFindWorkingCopy
	| constraintReference workingCopy |
	constraintReference := GoferConstraintReference 
		name: 'Gofer-Core'
		constraint: [ :reference | false ].
	workingCopy := constraintReference workingCopy.
	self assert: workingCopy packageName = 'Gofer-Core'