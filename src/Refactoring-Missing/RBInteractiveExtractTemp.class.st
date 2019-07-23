"
I am a interactive layer to  use extract temp refactoring

Instance Variables
-----------------------------------------------------------------
tempName: aString
"
Class {
	#name : #RBInteractiveExtractTemp,
	#superclass : #RBInteractiveRefactoring,
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
