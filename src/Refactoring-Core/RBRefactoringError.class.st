"
The receiver is signaled whenever a precondition of a refactoring is violated.
"
Class {
	#name : 'RBRefactoringError',
	#superclass : 'Error',
	#instVars : [
		'failedPreconditions'
	],
	#category : 'Refactoring-Core-Support',
	#package : 'Refactoring-Core',
	#tag : 'Support'
}

{ #category : 'signalling' }
RBRefactoringError class >> signalFor: failingConditions [
	| errorStrings |
	errorStrings := String streamContents: [ :aStream |
		                failingConditions do: [ :cond |
			                cond violationMessageOn: aStream ] ].
	
	^ self new
		failedPreconditions: failingConditions;
		messageText: errorStrings;
		signal	
]

{ #category : 'accessing' }
RBRefactoringError >> failedPreconditions [
	^ failedPreconditions
]

{ #category : 'accessing' }
RBRefactoringError >> failedPreconditions: aCollection [
	"aCollection should contain objects of kind RBNewAbstractCondition"
	failedPreconditions := aCollection
]
