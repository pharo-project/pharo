preconditions
verifySelectedInterval
	| selectedParseTree selectedSources |
	selectedSources := self selectedSource.
	selectedParseTree := RBParser parseExpression: selectedSources
				onError: [:message :position | self refactoringFailure: 'Invalid selection'].
	selectedParseTree isSequence 
		ifTrue: [self refactoringFailure: 'Cannot assign temp to multiple statements']