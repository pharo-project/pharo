testing
testMissingArgumentAfterABinaryMessage
	self setUpForErrorsIn: '1 +` Variable or expression expected ->`'.
	self enumerateAllSelections.
	self setUpForErrorsIn: '1 + ` Variable or expression expected ->`* 2 + 3'.
	self enumerateAllSelections