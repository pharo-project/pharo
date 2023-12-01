"
The receiver is signaled whenever a precondition of a refactoring is violated.
"
Class {
	#name : 'RBRefactoringError',
	#superclass : 'Error',
	#instVars : [
		'failedConditions'
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
		faledConditions: failingConditions;
		messageText: errorStrings;
		signal	
]
