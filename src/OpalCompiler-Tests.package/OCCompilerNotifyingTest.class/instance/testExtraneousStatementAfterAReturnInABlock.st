testing
testExtraneousStatementAfterAReturnInABlock
	self setUpForErrorsIn: '[ ^1 ` End of statement list encountered ->`2]'.
	self enumerateAllSelections.