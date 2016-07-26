testing
testUnmatchedExpressionParenthesis

	self setUpForErrorsIn: '1+(2 ` '')'' expected ->`. '.
	self enumerateAllSelections.