testing
testExpectedExpressionInBraceArray
	
	self setUpForErrorsIn: '{ 1. 2 ` End of statement list encountered ->`3 }'.
	self enumerateAllSelections.
	self setUpForErrorsIn: '{ 1. 2. ` Variable or expression expected ->`| x | x}'.
	self enumerateAllSelections.