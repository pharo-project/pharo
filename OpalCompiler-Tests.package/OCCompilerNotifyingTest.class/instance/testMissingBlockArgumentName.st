testing
testMissingBlockArgumentName

	self setUpForErrorsIn: '[ :x : ` Variable name expected ->`1]'.
	self enumerateAllSelections