"
I'm a model for user interaction for the rename shared variable refactoring.
"
Class {
	#name : 'ReRenameSharedVariableDriver',
	#superclass : 'ReRenameVariableDriver',
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'factory method' }
ReRenameSharedVariableDriver >> refactoringClass [

	^ RBRenameClassVariableRefactoring
]
