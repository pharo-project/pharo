tests
testPass

	ReExceptionStrategy current: RePassExceptionStrategy.
	
	self
		should: [ self validationScenario  ]
		raise: MyTestError