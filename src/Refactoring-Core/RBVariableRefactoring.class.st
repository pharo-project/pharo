"
I am an abstract base class of refactorings modifying class or instance variables.
"
Class {
	#name : #RBVariableRefactoring,
	#superclass : #RBRefactoring,
	#instVars : [
		'class',
		'variableName'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBVariableRefactoring class >> model: aRBSmalltalk variable: aVarName class: aClass [ 
	^(self new)
		model: aRBSmalltalk;
		variable: aVarName class: aClass;
		yourself
]

{ #category : #'instance creation' }
RBVariableRefactoring class >> variable: aVarName class: aClass [
	^self new variable: aVarName class: aClass
]

{ #category : #printing }
RBVariableRefactoring >> storeOn: aStream [ 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' variable: '.
	variableName storeOn: aStream.
	aStream nextPutAll: ' class: '.
	class storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #initialization }
RBVariableRefactoring >> variable: aVarName class: aClass [ 
	class := self classObjectFor: aClass.
	variableName := aVarName
]
