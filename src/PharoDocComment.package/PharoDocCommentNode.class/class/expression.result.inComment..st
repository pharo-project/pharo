instance creation
expression: aDocExpression result: anotherDocExpression inComment: aRBComment
	^ self new
		expression: aDocExpression;
		result: anotherDocExpression;
		sourceNode: aRBComment
		yourself