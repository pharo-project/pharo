"
I am responsible for adding a pragma inside a method. In this implementation, all pragma definition, e.g., '<selector: args>' must be described.

Usage:
| transformation |
transformation := (RBAddPragmaTransformation
				pragma: '<gtInspectorPresentationOrder: 213>'
				inMethod: #methodBefore
				inClass: #RBAddPragmaTransformationTest)
				transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
- the class exists,
- the method exists, and
- the method does not have a pragma with the same selector. 
"
Class {
	#name : #RBAddPragmaTransformation,
	#superclass : #RBMethodTransformation,
	#instVars : [
		'pragma'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBAddPragmaTransformation class >> model: aRBModel pragma: aString inMethod: aSelector inClass: aClassName [

	^ self new
		model: aRBModel;
		pragma: aString
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : #api }
RBAddPragmaTransformation class >> pragma: aString inMethod: aSelector inClass: aClassName [

	^ self new
		pragma: aString
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : #api }
RBAddPragmaTransformation >> pragma: aString inMethod: aSelector inClass: aClassName [

	self className: aClassName.
	selector := aSelector.
	pragma := aString.
]

{ #category : #preconditions }
RBAddPragmaTransformation >> preconditions [
	^ (RBCondition
		withBlock: [ self definingClass isNotNil ]
		errorString: ('Class named <1s> does not exist' expandMacrosWith: className))
		&
			(RBCondition
				withBlock: [ self definingClass canUnderstand: selector ]
				errorString: ('Method named <1s> does not exist' expandMacrosWith: selector))
		&
			(RBCondition
				withBlock: [ | pragmaNode |
					pragmaNode := self parserClass parsePragma: pragma.
					self definingMethod pragmas
						noneSatisfy: [ :e | e selector = pragmaNode selector ] ]
				errorString:
					('Method named <1s> already has a pragma named <2s>'
						expandMacrosWith: selector
						with: (self parserClass parsePragma: pragma) selector))
]

{ #category : #executing }
RBAddPragmaTransformation >> privateTransform [
	"the transformation is straighforward: just add the node at the last position of the method.
	 #todo the case in which the context is a block"

	| methodTree pragmaNode |
	methodTree := self definingMethod.
	pragmaNode := self parserClass parsePragma: pragma.
	methodTree addPragma: pragmaNode.
	self definingClass compileTree: methodTree
]

{ #category : #printing }
RBAddPragmaTransformation >> storeOn: aStream [ 

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' pragma: ''';
		nextPutAll: pragma;
		nextPutAll: ''' inMethod: ''';
		nextPutAll: selector;
		nextPutAll: ''' inClass: '.
	class storeOn: aStream.
	aStream nextPut: $)
]
