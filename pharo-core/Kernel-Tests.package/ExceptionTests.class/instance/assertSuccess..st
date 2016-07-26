private
assertSuccess: anExceptionTester
	self should: [ ( anExceptionTester suiteLog first) endsWith:  'succeeded'].