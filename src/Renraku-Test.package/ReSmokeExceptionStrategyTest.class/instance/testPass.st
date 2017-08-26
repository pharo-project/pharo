tests
testPass

	ReExceptionStrategy current: RePassExceptionStrategy.
	
	self should: [ thisContext method critiques ]
		raise: MyTestError.