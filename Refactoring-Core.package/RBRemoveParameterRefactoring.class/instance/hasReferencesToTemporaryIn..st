transforming
hasReferencesToTemporaryIn: each 
	| tree |
	tree := each parseTreeFor: oldSelector.
	tree isNil ifTrue: [self refactoringFailure: 'Cannot parse sources.'].
	^tree references: (tree argumentNames at: parameterIndex)