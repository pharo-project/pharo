"
I'm a model for user interaction for the rename instance variable refactoring.
"
Class {
	#name : 'RBRenameInstanceVariableDriver',
	#superclass : 'RBRenameVariableDriver',
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'factory method' }
RBRenameInstanceVariableDriver >> refactoringClass [

	^ RBRenameInstanceVariableRefactoring
]
