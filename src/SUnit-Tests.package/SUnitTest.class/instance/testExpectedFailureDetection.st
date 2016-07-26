testing
testExpectedFailureDetection
	self assert: self expectedFailures size = 2.
	self assert: (self expectedFailures includesAllOf: #(expectedFailureFails expectedFailurePasses))