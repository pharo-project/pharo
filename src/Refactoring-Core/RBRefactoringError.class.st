"
The receiver is signaled whenever a precondition of a refactoring is violated. The action block, if defined, might help the user to resolve the issue.
"
Class {
	#name : #RBRefactoringError,
	#superclass : #Error,
	#instVars : [
		'actionBlock'
	],
	#category : 'Refactoring-Core-Support'
}

{ #category : #signalling }
RBRefactoringError class >> signal: aString with: aBlock [ 
	^ self new
		actionBlock: aBlock;
		signal: aString
]

{ #category : #accessing }
RBRefactoringError >> actionBlock [
	^ actionBlock
	
]

{ #category : #accessing }
RBRefactoringError >> actionBlock: aBlock [ 
	actionBlock := aBlock
]

{ #category : #testing }
RBRefactoringError >> isResumable [
	^ true
]
