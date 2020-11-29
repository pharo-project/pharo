"
I model the fact that a test generated an error.
"
Class {
	#name : #DTError,
	#superclass : #DTTestResultType,
	#category : #'DrTests-TestsRunner-Results'
}

{ #category : #factory }
DTError class >> backgroundColor [
	^ TestResult defaultColorBackGroundForErrorTest
]

{ #category : #testing }
DTError >> isError [
	<generated>
	^ true
]

{ #category : #accessing }
DTError >> name [
	^ 'Error'
]
