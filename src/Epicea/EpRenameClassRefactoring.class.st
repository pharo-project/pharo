"
I represent the execution of a ""rename class"" (RBRenameClassRefactoring) refactoring.
"
Class {
	#name : #EpRenameClassRefactoring,
	#superclass : #EpRefactoring,
	#instVars : [
		'newName',
		'oldName'
	],
	#category : #'Epicea-Model'
}

{ #category : #'instance creation' }
EpRenameClassRefactoring class >> rename: oldName to: newName [
	^ self basicNew
		initializeFrom: oldName
		to: newName;
		yourself
]

{ #category : #converting }
EpRenameClassRefactoring >> asRBRefactoring [

	^ RBRenameClassRefactoring new 
		className: oldName 
		newName: newName
]

{ #category : #initialization }
EpRenameClassRefactoring >> initializeFrom: anOldName to: aNewName [
	self initialize.
	
	oldName := anOldName.
	newName := aNewName.
]

{ #category : #accessing }
EpRenameClassRefactoring >> newName [
	^ newName
]

{ #category : #accessing }
EpRenameClassRefactoring >> oldName [
	^ oldName
]
