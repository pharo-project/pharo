Class {
	#name : #RBInteractiveExtractTemp,
	#superclass : #RBGenericInteractLayer,
	#instVars : [
		'tempName'
	],
	#category : #'Refactoring-Missing'
}

{ #category : #execution }
RBInteractiveExtractTemp >> executeExtractTemp [
	self
		executeThis: [ refactoring := self extractTempWithName: tempName.
			self executeRefactoringWith: #primitiveExecute ]
]

{ #category : #execution }
RBInteractiveExtractTemp >> executionOfRequirements [
	tempName := UIManager default 
		request: 'New temp name' 
		initialAnswer: ''
		title: 'Extract code to temp variable'.
	tempName isEmptyOrNil ifTrue: [ RBRefactoringError signal ]
]
