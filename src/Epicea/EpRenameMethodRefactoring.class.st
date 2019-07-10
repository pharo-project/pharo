"
I represent the execution of a ""rename method"" (RBRenameMethodRefactoring) refactoring.
"
Class {
	#name : #EpRenameMethodRefactoring,
	#superclass : #EpRefactoring,
	#instVars : [
		'oldSelector',
		'newSelector',
		'affectedClassName'
	],
	#category : #'Epicea-Model'
}

{ #category : #'instance creation' }
EpRenameMethodRefactoring class >> renameMethod: oldSelector in: affectedClassName to: newSelector [
	^ self basicNew
		initializeFrom: oldSelector 
		to: newSelector 
		in: affectedClassName;
		yourself
]

{ #category : #converting }
EpRenameMethodRefactoring >> asRBRefactoring [
	^ RBRenameMethodRefactoring
		renameMethod: oldSelector 
		in: affectedClassName 
		to: newSelector 
		permutation: #()
]

{ #category : #initialization }
EpRenameMethodRefactoring >> initializeFrom: anOldSelector to: aNewSelector in: aClassName [
	self initialize.
	
	oldSelector := anOldSelector.
	newSelector := aNewSelector.
	affectedClassName := aClassName
]

{ #category : #accessing }
EpRenameMethodRefactoring >> newSelector [
	^ newSelector
]

{ #category : #accessing }
EpRenameMethodRefactoring >> oldSelector [
	^ oldSelector
]
