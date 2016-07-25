testing
testUnmatchedLocalTempDeclaration

	self setUpForErrorsIn: '| x y ` ''|'' expected ->`'.
	self enumerateAllSelections.