testing
testMissingPeriodSeparatorBetweenStatements

	self setUpForErrorsIn: '1 + 2 ` End of statement list encountered ->`^nil'.
	self enumerateAllSelections.