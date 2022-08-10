"
I model the fact that a tests expected to fail failed
"
Class {
	#name : #DTExpectedFailureResultType,
	#superclass : #DTTestResultType,
	#category : #'DrTests-TestsRunner-Results'
}

{ #category : #factory }
DTExpectedFailureResultType class >> backgroundColor [
	^ TestResult defaultColorBackGroundForExpectedFailureTest
]

{ #category : #accessing }
DTExpectedFailureResultType >> isExpectedFailure [
	^ true
]

{ #category : #accessing }
DTExpectedFailureResultType >> name [
	^ 'ExpectedFailure'
]
