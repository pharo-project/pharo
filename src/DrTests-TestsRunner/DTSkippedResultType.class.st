"
I model the fact that a test was skipped.
"
Class {
	#name : #DTSkippedResultType,
	#superclass : #DTTestResultType,
	#category : #'DrTests-TestsRunner-Results'
}

{ #category : #factory }
DTSkippedResultType class >> backgroundColor [
	^ TestResult defaultColorBackGroundForPassingTest
]

{ #category : #factory }
DTSkippedResultType class >> backgroundColorStyle [

	^ 'testSkipped'
]

{ #category : #testing }
DTSkippedResultType >> isSkipped [
	^ true
]

{ #category : #accessing }
DTSkippedResultType >> name [
	^ 'Skipped test'
]
