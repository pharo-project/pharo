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

{ #category : 'notification' }
RBRefactoringError >> notifyUserOfCommand: aCommand [
	"This is part of Commander2. For now I live in refactorings because the dependencies are not yet clean. In the future Commander2 should depend on refactorings and this method should be an extension."

	InformativeNotification signal: messageText
]
