Class {
	#name : #RBRefactoringChangeMock,
	#superclass : #Object,
	#instVars : [
		'instVar'
	],
	#classVars : [
		'ClassVar'
	],
	#classInstVars : [
		'classInstVar'
	],
	#category : #'Refactoring-Tests-Changes'
}

{ #category : #accessing }
RBRefactoringChangeMock >> accessAll [ 
	instVar := 0. 
	ClassVar := 0.
	^ instVar + ClassVar 
]

{ #category : #accessing }
RBRefactoringChangeMock >> one [

	^ 1
]
