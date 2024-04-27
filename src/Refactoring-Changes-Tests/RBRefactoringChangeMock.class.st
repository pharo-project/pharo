"
This class is used to test refactoring changes.
"
Class {
	#name : 'RBRefactoringChangeMock',
	#superclass : 'RBRefactoryChange',
	#instVars : [
		'instVar'
	],
	#classVars : [
		'SharedVar'
	],
	#classInstVars : [
		'classInstVar'
	],
	#category : 'Refactoring-Changes-Tests',
	#package : 'Refactoring-Changes-Tests'
}

{ #category : 'accessing' }
RBRefactoringChangeMock class >> classInsVar [

	^ classInstVar
]

{ #category : 'accessing' }
RBRefactoringChangeMock >> accessAll [ 
	instVar := 0. 
	SharedVar := 0.
	^ instVar + SharedVar 
]

{ #category : 'private' }
RBRefactoringChangeMock >> executeNotifying: aBlock [
	"I'm a mock, I do nothing"
	^ self
]

{ #category : 'initialization' }
RBRefactoringChangeMock >> initialize [

	<ignoreUnusedVariables: #( #instVar )>
	super initialize
]

{ #category : 'accessing' }
RBRefactoringChangeMock >> one [

	^ 1
]

{ #category : 'printing' }
RBRefactoringChangeMock >> printOn: aStream [
	"Generate a string representation of the receiver based on its instance variables."

	super printOn: aStream.
	aStream
		nextPutAll: ' name: ';
		print: name
]

{ #category : 'accessing' }
RBRefactoringChangeMock >> renameChangesForClass: oldClassName to: newClassName [
	"I'm a mock, I do nothing"
	^ self
]
