"
I open the debuger on the first TestFailure encontered while executing a TestSuite.
I always returns an empty result (TestResult). 
"
Class {
	#name : 'DebugExecutionMode',
	#superclass : 'AbstractExecutionMode',
	#category : 'TestRunner-Core-ExecutionModes',
	#package : 'TestRunner-Core',
	#tag : 'ExecutionModes'
}

{ #category : 'execution' }
DebugExecutionMode class >> executeCase: aTestCase [

	"executes the giving test case. Notice that if the test failed, the result is not returned"

	^ aTestCase debug
]
