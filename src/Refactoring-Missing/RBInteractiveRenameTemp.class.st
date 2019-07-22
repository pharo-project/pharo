Class {
	#name : #RBInteractiveRenameTemp,
	#superclass : #RBGenericInteractLayer,
	#instVars : [
		'newName'
	],
	#category : #'Refactoring-Missing'
}

{ #category : #execution }
RBInteractiveRenameTemp >> executeRenameTemp [
	self
		executeThis: [ refactoring := self renameTempWithName: newName.
			self executeRefactoringWith: #primitiveExecute ]
]

{ #category : #execution }
RBInteractiveRenameTemp >> executionOfRequirements [
	newName := UIManager default 
		request: 'New name of the variable' 
		initialAnswer: sourceNode name
		title: 'Rename a temp variable'.
	newName isEmptyOrNil | (newName = sourceNode name) ifTrue: [ RBRefactoringError signal ]
]
