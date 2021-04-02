"
I model the fact that a test generated an error.
"
Class {
	#name : #DTErrorTestResult,
	#superclass : #DTAbstractTestResult,
	#category : #'DrTests-TestsRunner-Results'
}

{ #category : #factory }
DTErrorTestResult class >> backgroundColor [
	^ TestResult defaultColorBackGroundForErrorTest
]

{ #category : #testing }
DTErrorTestResult >> isError [
	<generated>
	^ true
]

{ #category : #accessing }
DTErrorTestResult >> name [
	^ 'Error'
]
