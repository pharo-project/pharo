"
I ececute a test suite then return its TestResult.
"
Class {
	#name : #RunExecutionMode,
	#superclass : #AbstractExecutionMode,
	#category : #'TestRunner-Core-Configuration'
}

{ #category : #execution }
RunExecutionMode class >> executeCase: aTestCase [

	aTestCase class isTestCase ifFalse: [ ^ TestResult new ].
	^ aTestCase run
]

{ #category : #execution }
RunExecutionMode class >> executeSuite: aTestSuite [

	^ aTestSuite run
]
