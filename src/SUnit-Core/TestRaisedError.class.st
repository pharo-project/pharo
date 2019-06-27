"
I model the fact that a test raised an exception that is not an AssertionFailure.
"
Class {
	#name : #TestRaisedError,
	#superclass : #TestResultType,
	#category : #'SUnit-Core-Kernel'
}

{ #category : #accessing }
TestRaisedError >> color [
	^ self theme dangerBackgroundColor
]

{ #category : #testing }
TestRaisedError >> isRaisedError [
	^ true
]
