preconditions
addStatementNode: aNode
	aNode isMessage
		ifTrue: [ ^ statementNodes add: aNode ].
	aNode isCascade
		ifTrue: [ ^ statementNodes addAll: aNode messages ].
	self refactoringFailure: aNode formattedCode , ' is not a valid message'