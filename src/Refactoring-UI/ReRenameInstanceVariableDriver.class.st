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

{ #category : 'execution' }
ReRenameInstanceVariableDriver >> configureRefactoring [

	super configureRefactoring.
	refactoring renameAccessors: true
]

{ #category : 'factory method' }
ReRenameInstanceVariableDriver >> refactoringClass [

	^ RBRenameInstanceVariableRefactoring
]
