tests
testStringConversionJustLF

	| string newString |
	string := 'This is a test', String lf, 'of the conversion'.
	newString := string withSqueakLineEndings.
	self assert: (string size = newString size).