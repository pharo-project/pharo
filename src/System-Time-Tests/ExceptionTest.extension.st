Extension { #name : 'ExceptionTest' }

{ #category : '*System-Time-Tests' }
ExceptionTest >> testNoTimeout [
	self assertSuccess: (ExceptionTester new runTest: #simpleNoTimeoutTest )
]

{ #category : '*System-Time-Tests' }
ExceptionTest >> testTimeoutWithZeroDuration [
	self assertSuccess: (ExceptionTester new runTest: #simpleTimeoutWithZeroDurationTest )
]
