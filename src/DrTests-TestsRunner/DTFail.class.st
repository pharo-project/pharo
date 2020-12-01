"
I model the fact that a test failed.
"
Class {
	#name : #DTFail,
	#superclass : #DTTestResultType,
	#category : #'DrTests-TestsRunner-Results'
}

{ #category : #factory }
DTFail class >> backgroundColor [
	^ TestResult defaultColorBackGroundForFailureTest
]

{ #category : #testing }
DTFail >> isFail [
	<generated>
	^ true
]

{ #category : #accessing }
DTFail >> name [
	^ 'Failure'
]
