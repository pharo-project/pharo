testing
testUnmatchedLocalTempDeclarationInABlock

	self setUpForErrorsIn: '[:z | | x y ` ''|'' expected ->`]'.
	self enumerateAllSelections.