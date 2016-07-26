actions
runAll

	self resetResult.
	self runTestSuites: self allSelectedTestSuites.
	self saveResultInHistory