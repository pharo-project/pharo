transforming
findSelectedMessage
	sourceParseTree := class parseTreeFor: sourceSelector.
	sourceParseTree isNil 
		ifTrue: [self refactoringFailure: 'Could not parse sources'].
	sourceMessage := sourceParseTree whichNodeIsContainedBy: sourceInterval.
	sourceMessage isNil 
		ifTrue: 
			[self 
				refactoringFailure: 'The selection doesn''t appear to be a message send'].
	sourceMessage isCascade 
		ifTrue: [sourceMessage := sourceMessage messages last].
	sourceMessage isMessage 
		ifFalse: 
			[self 
				refactoringFailure: 'The selection doesn''t appear to be a message send'].
	(sourceMessage receiver isVariable 
		and: [#('self' 'super') includes: sourceMessage receiver name]) 
			ifFalse: [self refactoringError: 'Cannot inline non-self messages']