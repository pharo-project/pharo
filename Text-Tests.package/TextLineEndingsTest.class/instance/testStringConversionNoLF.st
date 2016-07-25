tests
testStringConversionNoLF

	| string newString |
	string := 'This is a test', String cr, 'of the conversion'.
	newString := string withSqueakLineEndings.
	self assert: (string = newString).