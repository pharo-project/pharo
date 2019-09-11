"
I am a refactoring for generating a printString (printOn: aStream) method. 

You can specify which of my instance variables should be used for generating the printString.

For example: 
RBGeneratePrintStringRefactoring model:RBNamespace new className: ClassS variables: { #inst1 . #inst2 . #inst3 }.

"
Class {
	#name : #RBGeneratePrintStringRefactoring,
	#superclass : #RBClassRefactoring,
	#instVars : [
		'variables'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBGeneratePrintStringRefactoring class >> className: aClass variables: anArray [
	^ (self className: aClass) variables: anArray
]

{ #category : #'instance creation' }
RBGeneratePrintStringRefactoring class >> model: aNamespace className: aClass variables: anArray [
	^ (self model: aNamespace className: aClass) variables: anArray
]

{ #category : #preconditions }
RBGeneratePrintStringRefactoring >> preconditions [
	^ variables
		inject: RBCondition empty
		into: [ :condition :variable | condition & (RBCondition definesInstanceVariable: variable in: self theClass) ]
]

{ #category : #accessing }
RBGeneratePrintStringRefactoring >> theClass [
	^ (self classObjectFor: className) instanceSide
]

{ #category : #transforming }
RBGeneratePrintStringRefactoring >> transform [
	| method |
	method := self parserClass
		parseMethod:
			'printOn: aStream
		"Append a sequence of characters to aStream that identify the receiver."
		
		super printOn: aStream'.
	variables
		do: [ :each | 
			method body
				addNode:
					(self parserClass
						parseExpression:
							('aStream nextPutAll: '' <1s>: ''; print: <1s>' expandMacrosWith: each)) ].
	self theClass compile: method formattedCode classified: #(printing)
]

{ #category : #accessing }
RBGeneratePrintStringRefactoring >> variables: anArray [
	variables := anArray
]
