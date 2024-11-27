"
I am an abstract base class of refactorings modifying class or instance variables.
"
Class {
	#name : 'RBVariableRefactoring',
	#superclass : 'ReRefactoring',
	#instVars : [
		'class',
		'variableName'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'testing' }
RBVariableRefactoring class >> isAbstract [

	^ self == RBVariableRefactoring
]

{ #category : 'instance creation' }
RBVariableRefactoring class >> model: aRBSmalltalk variable: aVarName class: aClass [
	^ self new
		model: aRBSmalltalk;
		variable: aVarName class: aClass;
		yourself
]

{ #category : 'instance creation' }
RBVariableRefactoring class >> variable: aVarName class: aClass [
	^ self new
		variable: aVarName class: aClass;
		yourself
]

{ #category : 'initialization' }
RBVariableRefactoring >> refactoredClassName [
	^ class name
]

{ #category : 'storing' }
RBVariableRefactoring >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' variable: '.
	variableName storeOn: aStream.
	aStream nextPutAll: ' class: '.
	class storeOn: aStream.
	aStream nextPut: $)
]

{ #category : 'initialization' }
RBVariableRefactoring >> variable: aVarName class: aClassName [
	class := self classObjectFor: aClassName.
	variableName := aVarName
]

{ #category : 'accessing' }
RBVariableRefactoring >> variableName [

	^ variableName
]
