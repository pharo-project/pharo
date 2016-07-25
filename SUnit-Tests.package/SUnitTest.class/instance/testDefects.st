testing
testDefects
	| result suite error failure |
	suite := self classForTestSuite new.
	suite addTest: (error := self class selector: #error).
	suite addTest: (failure := self class selector: #fail).
	result := suite run.
	self assert: result defects asArray = (Array with: error with: failure).
	self
		assertForTestResult: result
		runCount: 2
		passed: 0
		failed: 1
		errors: 1