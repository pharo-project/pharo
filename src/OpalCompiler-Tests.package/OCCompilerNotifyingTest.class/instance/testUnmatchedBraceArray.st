testing
testUnmatchedBraceArray
	self setUpForErrorsIn: '{ 1. 2` expected } ->`'.
	self enumerateAllSelections.
	self setUpForErrorsIn: '{ 1. 2 ` expected } ->`'.
	self enumerateAllSelections