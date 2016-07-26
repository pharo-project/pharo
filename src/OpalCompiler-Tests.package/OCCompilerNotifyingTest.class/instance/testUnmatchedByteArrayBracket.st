testing
testUnmatchedByteArrayBracket

	self setUpForErrorsIn: '#[ 1 2 ` '']'' expected ->`'.
	self enumerateAllSelections