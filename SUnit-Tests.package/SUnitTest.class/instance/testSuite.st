testing
testSuite
	| suite result |
	suite := self classForTestSuite new.
	suite
		addTest: (self class selector: #noop);
		addTest: (self class selector: #fail);
		addTest: (self class selector: #error).
	result := suite run.
	self
		assertForTestResult: result
		runCount: 3
		passed: 1
		failed: 1
		errors: 1