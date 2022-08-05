"
I model the fact that a test expected to fail passed
"
Class {
	#name : #DTUnexpectedPassResultType,
	#superclass : #DTTestResultType,
	#category : #'DrTests-TestsRunner-Results'
}

{ #category : #factory }
DTUnexpectedPassResultType class >> backgroundColor [
	^ TestResult defaultColorBackGroundForUnexpectedPassTest
]

{ #category : #accessing }
DTUnexpectedPassResultType >> isUnexpectedPass [

	^ true
]

{ #category : #accessing }
DTUnexpectedPassResultType >> name [
	^ 'Unexpected passed test'
]
