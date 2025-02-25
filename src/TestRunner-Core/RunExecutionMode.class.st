"
I ececute a test suite then return its TestResult.
"
Class {
	#name : 'RunExecutionMode',
	#superclass : 'AbstractExecutionMode',
	#category : 'TestRunner-Core-ExecutionModes',
	#package : 'TestRunner-Core',
	#tag : 'ExecutionModes'
}

{ #category : 'execution' }
RunExecutionMode class >> executeCase: aTestCase [


	"executes the giving test case then returns the result"


	^ aTestCase run
]
