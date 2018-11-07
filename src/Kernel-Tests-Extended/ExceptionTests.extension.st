Extension { #name : #ExceptionTests }

{ #category : #'*Kernel-Tests-Extended' }
ExceptionTests >> testSimpleRetry [
	self assertSuccess: (ExceptionTester new runTest: #simpleRetryTest ) 
]

{ #category : #'*Kernel-Tests-Extended' }
ExceptionTests >> testSimpleRetryUsing [
	self assertSuccess: (ExceptionTester new runTest: #simpleRetryUsingTest ) 
]
