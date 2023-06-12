"
This class is used to test refactoring changes.
"
Class {
	#name : #RBRefactoringChangeMock,
	#superclass : #Object,
	#instVars : [
		'instVar'
	],
	#classVars : [
		'SharedVar'
	],
	#classInstVars : [
		'SharedVarPlus',
		'instVarPlus',
		'classInstVar'
	],
	#category : #'Refactoring-Tests-Changes'
}

{ #category : #accessing }
RBRefactoringChangeMock class >> classInsVar [

	^ classInstVar
]

{ #category : #accessing }
RBRefactoringChangeMock >> accessAll [ 
	instVar := 0. 
	SharedVar := 0.
	^ instVar + SharedVar 
]

{ #category : #accessing }
RBRefactoringChangeMock >> one [

	^ 1
]
