"
The receiver is a warning that usually requires the user to validate. This is used in situations where either the behavior of the program will not be strictly preserved or the change may have a wider impact than the user may think.
The action block, if defined, might help the user to resolve the issue.
"
Class {
	#name : 'RBRefactoringWarning',
	#superclass : 'Notification',
	#instVars : [
		'actionBlock'
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

{ #category : 'accessing' }
RBRefactoringWarning >> actionBlock [

	^ actionBlock
]

{ #category : 'accessing' }
RBRefactoringWarning >> actionBlock: anObject [

	actionBlock := anObject
]
