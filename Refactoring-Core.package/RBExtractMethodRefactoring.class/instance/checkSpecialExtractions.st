transforming
checkSpecialExtractions
	| node |
	node := self placeholderNode parent.
	node isNil ifTrue: [^self].
	(node isAssignment and: [node variable = self placeholderNode]) ifTrue: 
			[self refactoringFailure: 'Cannot extract left hand side of an assignment'].
	node isCascade ifTrue: 
			[self refactoringError: 'Cannot extract first message of a cascaded message']