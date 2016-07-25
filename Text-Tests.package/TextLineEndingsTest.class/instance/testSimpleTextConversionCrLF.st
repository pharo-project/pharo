tests
testSimpleTextConversionCrLF

	| string newText |
	string := 'This is a test', String crlf, 'of the conversion'.
	newText := string asText withSqueakLineEndings.
	self assert: ((string size - 1) = newText size).
	self assert: (newText size = newText runs size).