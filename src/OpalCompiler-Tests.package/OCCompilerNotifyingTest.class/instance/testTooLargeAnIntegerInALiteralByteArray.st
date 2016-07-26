testing
testTooLargeAnIntegerInALiteralByteArray

	self setUpForErrorsIn: '#[ 1 2 ` Expecting 8-bit integer ->`256 4 5]'.
	self enumerateAllSelections