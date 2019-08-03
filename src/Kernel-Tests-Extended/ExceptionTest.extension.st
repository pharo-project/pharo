Extension { #name : #ExceptionTest }

{ #category : #'*Kernel-Tests-Extended' }
ExceptionTest >> testSimpleRetry [
	self assertSuccess: (ExceptionTester new runTest: #simpleRetryTest ) 
]

{ #category : #'*Kernel-Tests-Extended' }
ExceptionTest >> testSimpleRetryUsing [
	self assertSuccess: (ExceptionTester new runTest: #simpleRetryUsingTest ) 
]
