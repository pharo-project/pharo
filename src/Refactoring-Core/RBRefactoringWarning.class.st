"
The receiver is a warning that usually requires the user to validate. This is used in situations where either the behavior of the program will not be strictly preserved or the change may have a wider impact than the user may think.
"
Class {
	#name : #RBRefactoringWarning,
	#superclass : #Notification,
	#instVars : [
		'actionBlock'
	],
	#category : #'Refactoring-Core-Support'
}

{ #category : #accessing }
RBRefactoringWarning >> actionBlock [

	^ actionBlock
]

{ #category : #accessing }
RBRefactoringWarning >> actionBlock: anObject [

	actionBlock := anObject
]
