testing
testMissingExpression

	
	self setUpForErrorsIn: '| x | x := ` Variable or expression expected ->'.
	self enumerateAllSelections