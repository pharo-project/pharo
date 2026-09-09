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

{ #category : 'accessing' }
ReRenameInstanceVariableDriver class >> driverName [

	^ 'Rename instance variable'
]

{ #category : 'execution' }
ReRenameInstanceVariableDriver >> configureRefactoring [

	super configureRefactoring.
	refactoring renameAccessors: false
]

{ #category : 'factory method' }
ReRenameInstanceVariableDriver >> refactoringClass [

	^ RBRenameInstanceVariableRefactoring
]
