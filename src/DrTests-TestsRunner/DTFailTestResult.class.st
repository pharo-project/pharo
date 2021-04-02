"
I model the fact that a test failed.
"
Class {
	#name : #DTFailTestResult,
	#superclass : #DTAbstractTestResult,
	#category : #'DrTests-TestsRunner-Results'
}

{ #category : #factory }
DTFailTestResult class >> backgroundColor [
	^ TestResult defaultColorBackGroundForFailureTest
]

{ #category : #testing }
DTFailTestResult >> isFail [
	<generated>
	^ true
]

{ #category : #accessing }
DTFailTestResult >> name [
	^ 'Failure'
]
