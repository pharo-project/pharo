actions
runTest: aTestCase
	aTestCase run: result.
	self updateStatus: true.