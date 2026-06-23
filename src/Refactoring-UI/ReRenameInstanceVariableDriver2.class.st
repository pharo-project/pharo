"
I'm a model for user interaction for the rename instance variable refactoring.
"
Class {
	#name : 'ReRenameInstanceVariableDriver2',
	#superclass : 'ReRenameVariableDriver',
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReRenameInstanceVariableDriver2 class >> driverName [

	^ 'Rename instance variable'
]

{ #category : 'execution' }
ReRenameInstanceVariableDriver2 >> configureRefactoring [

	super configureRefactoringWithTemp.
	refactoring renameAccessors: false
]

{ #category : 'factory method' }
ReRenameInstanceVariableDriver2 >> refactoringClass [

	^ RBRenameInstanceVariableRefactoring
]
