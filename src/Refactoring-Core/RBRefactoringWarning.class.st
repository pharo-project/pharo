"
The receiver is a warning that usually requires the user to validate. This is used in situations where either the behavior of the program will not be strictly preserved or the change may have a wider impact than the user may think.
The action block, if defined, might help the user to resolve the issue.
"
Class {
	#name : 'RBRefactoringWarning',
	#superclass : 'Notification',
	#instVars : [
		'actionBlock',
		'failedConditions'
	],
	#category : 'Refactoring-Core-Support',
	#package : 'Refactoring-Core',
	#tag : 'Support'
}

{ #category : 'signalling' }
RBRefactoringWarning class >> signal: aString with: aBlock [
	^ self new
		actionBlock: aBlock;
		signal: aString
]

{ #category : 'signalling' }
RBRefactoringWarning class >> signalFor: failingConditions [
	| errorStrings |
	errorStrings := String streamContents: [ :aStream |
		                failingConditions do: [ :cond |
			                cond violationMessageOn: aStream ] ].
	
	^ self new
		faledConditions: failingConditions;
		messageText: errorStrings;
		signal	
]

{ #category : 'accessing' }
RBRefactoringWarning >> actionBlock [

	^ actionBlock
]

{ #category : 'accessing' }
RBRefactoringWarning >> actionBlock: anObject [

	actionBlock := anObject
]

{ #category : 'accessing' }
RBRefactoringWarning >> failedConditions [

	^ failedConditions
]

{ #category : 'accessing' }
RBRefactoringWarning >> failedConditions: anObject [

	failedConditions := anObject
]
