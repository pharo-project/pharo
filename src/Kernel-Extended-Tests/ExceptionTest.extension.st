Extension { #name : 'ExceptionTest' }

{ #category : '*Kernel-Extended-Tests' }
ExceptionTest >> testSimpleRetry [
	self assertSuccess: (ExceptionTester new runTest: #simpleRetryTest)
]

{ #category : '*Kernel-Extended-Tests' }
ExceptionTest >> testSimpleRetryUsing [
	self assertSuccess: (ExceptionTester new runTest: #simpleRetryUsingTest)
]
