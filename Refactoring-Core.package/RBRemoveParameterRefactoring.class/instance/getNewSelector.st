transforming
getNewSelector
	| tree |
	(class directlyDefinesMethod: oldSelector)
		ifFalse: [ self refactoringFailure: 'Method doesn''t exist' ].
	tree := class parseTreeFor: oldSelector.
	tree ifNil: [ self refactoringFailure: 'Cannot parse sources' ].
	argument ifNil:[ self refactoringFailure: 'This method does not have an argument' ].
	parameterIndex := tree argumentNames indexOf: argument ifAbsent: [ self refactoringFailure: 'Select a parameter!!' ].
	permutation := (1 to: oldSelector numArgs) copyWithout: parameterIndex.
	newSelector := self computeNewSelector