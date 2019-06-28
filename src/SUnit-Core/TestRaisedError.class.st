"
I model the fact that a test raised an exception that is not an AssertionFailure.
"
Class {
	#name : #TestRaisedError,
	#superclass : #TestResultSeverity,
	#category : #'SUnit-Core-Kernel'
}

{ #category : #testing }
TestRaisedError >> isRaisedError [
	^ true
]
