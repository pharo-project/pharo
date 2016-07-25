testing
testMissingSeparatorBetweenBlockArgumentAndStatements
	
	self setUpForErrorsIn: '[ :x ` ''|'' expected ->`x + 1 ]'.
	self enumerateAllSelections