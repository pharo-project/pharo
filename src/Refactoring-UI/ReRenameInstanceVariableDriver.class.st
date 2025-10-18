"
I'm a model for user interaction for the rename instance variable refactoring.
"
Class {
	#name : 'ReRenameInstanceVariableDriver',
	#superclass : 'ReRenameVariableDriver',
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'factory method' }
ReRenameInstanceVariableDriver >> refactoringClass [

	^ RBRenameInstanceVariableRefactoring
]
