testing
testInvalidLiteralCharacter
	self setUpForErrorsIn: '^ #yourself , #` Expecting a literal type ->`) , #end'.
	self enumerateAllSelections.