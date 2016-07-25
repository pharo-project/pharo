actions
debug: aTestCase
	self
		debugSuite:
			(self classForTestSuite new
				addTest: aTestCase;
				yourself)