"
I open the debuger on the first TestFailure encontered while executing a TestSuite.
I always returns an empty result (TestResult). 
"
Class {
	#name : #DebugExecutionMode,
	#superclass : #AbstractExecutionMode,
	#category : #'TestRunner-Core-Configuration'
}

{ #category : #execution }
DebugExecutionMode class >> executeCase: aTestCase [

	^ aTestCase debug
]

{ #category : #execution }
DebugExecutionMode class >> executeSuite: aTestSuite [

	| result |
	result := self defaultTestResultClass new.
	[ aTestSuite debug ] ensure: [ 
		aTestSuite unsubscribe: TestAnnouncement ].
	^ result
]
