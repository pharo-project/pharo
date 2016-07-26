tests
testStringConversionCrLF

	| string newString |
	string := 'This is a test', String crlf, 'of the conversion'.
	newString := string withSqueakLineEndings.
	self assert: ((string size - 1) = newString size).