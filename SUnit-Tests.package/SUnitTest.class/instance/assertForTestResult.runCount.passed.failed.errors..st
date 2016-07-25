private
assertForTestResult: aResult runCount: aRunCount passed: aPassedCount failed: aFailureCount errors: anErrorCount

	self
		assert: aResult runCount = aRunCount;
		assert: aResult passedCount = aPassedCount;
		assert: aResult failureCount = aFailureCount;
		assert: aResult errorCount = anErrorCount