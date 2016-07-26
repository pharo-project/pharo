testing
testAssignmentOfSelf

	self setUpForErrorsIn: '` Cannot store into ->`self := 1. ^self'.
	self enumerateAllSelections.