testing
testMissingExpressionAfterAReturn

	self setUpForErrorsIn: '^ ` Variable or expression expected ->`. 1 + 2'.
	self enumerateAllSelections.