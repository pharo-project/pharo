private
assertForTestResult: aResult runCount: aRunCount passed: aPassedCount failed: aFailureCount errors: anErrorCount expectedFailures: anExpectedFailureCount

	self
		assert: aResult runCount = aRunCount;
		assert: aResult expectedPassCount = aPassedCount;
		assert: aResult failureCount = aFailureCount;
		assert: aResult errorCount = anErrorCount;
		assert: aResult expectedDefectCount = anExpectedFailureCount