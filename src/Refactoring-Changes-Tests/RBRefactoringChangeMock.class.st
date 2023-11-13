"
This class is used to test refactoring changes.
"
Class {
	#name : 'RBRefactoringChangeMock',
	#superclass : 'Object',
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

{ #category : 'initialization' }
RBRefactoringChangeMock >> initialize [

	<ignoreUnusedVariables: #( #instVar )>
	super initialize
]

{ #category : 'accessing' }
RBRefactoringChangeMock >> one [

	^ 1
]
