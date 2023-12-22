"
I'm a model for user interaction for the rename shared variable refactoring.
"
Class {
	#name : 'RBRenameSharedVariableDriver',
	#superclass : 'RBRenameVariableDriver',
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'factory method' }
RBRenameSharedVariableDriver >> refactoringClass [

	^ RBRenameClassVariableRefactoring
]
