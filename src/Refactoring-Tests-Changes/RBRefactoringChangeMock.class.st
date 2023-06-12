Class {
	#name : #RBRefactoringChangeMock,
	#superclass : #Object,
	#instVars : [
		'instVar'
	],
	#classVars : [
		'ClassVarPlus',
		'SharedVar'
	],
	#classInstVars : [
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
