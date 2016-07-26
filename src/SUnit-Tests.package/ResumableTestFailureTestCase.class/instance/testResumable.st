running
testResumable
	| result suite |
	suite := self classForTestSuite new.
	suite addTest: (self class selector: #errorTest).
	suite addTest: (self class selector: #regularTestFailureTest).
	suite addTest: (self class selector: #resumableTestFailureTest).
	suite addTest: (self class selector: #okTest).
	result := suite run.
	self
		assert: result failures size = 2;
		assert: result errors size = 1