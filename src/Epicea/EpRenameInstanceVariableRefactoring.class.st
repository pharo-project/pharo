"
I represent the execution of a ""rename instance variable"" (RBRenameInstanceVariableRefactoring) refactoring.
"
Class {
	#name : #EpRenameInstanceVariableRefactoring,
	#superclass : #EpRefactoring,
	#instVars : [
		'newName',
		'oldName',
		'affectedClassName'
	],
	#category : 'Epicea-Model'
}

{ #category : #'instance creation' }
EpRenameInstanceVariableRefactoring class >> rename: oldName to: newName in: affectedClassName [
	^ self basicNew 
		initializeIn: affectedClassName
		from: oldName 
		to: newName;
		yourself
]

{ #category : #accessing }
EpRenameInstanceVariableRefactoring >> affectedClassName [
	^ affectedClassName
]

{ #category : #converting }
EpRenameInstanceVariableRefactoring >> asRBRefactoring [
	^ RBRenameInstanceVariableRefactoring 
		rename: oldName 
		to: newName 
		in: affectedClassName
]

{ #category : #initialize }
EpRenameInstanceVariableRefactoring >> initializeIn: aClassName from:  anOldName to: aNewName [
	self initialize.

	affectedClassName := aClassName.
	newName := aNewName.
	oldName := anOldName.
]

{ #category : #accessing }
EpRenameInstanceVariableRefactoring >> newName [
	^ newName
]

{ #category : #accessing }
EpRenameInstanceVariableRefactoring >> oldName [
	^ oldName
]
