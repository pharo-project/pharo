"
I am responsible for removing a pragma definition in a method. In this implementation, all the pragma (i.e., selector and arguments) must be described.

Usage:
| transformation |
transformation := (RBRemovePragmaTransformation
				pragma: '<gtInspectorPresentationOrder: 34>'
				inMethod: #methodBefore
				inClass: #RBRemovePragmaTransformationTest)
				transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
- the class exists,
- the methos exists, and
- this method has a pragma with the given selector.

Observations about the transformation:
- Although the pragma must be described, the transformation will remove the pragma that has the same selector as the specified one.
"
Class {
	#name : #RBRemovePragmaTransformation,
	#superclass : #RBMethodTransformation,
	#instVars : [
		'pragma'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBRemovePragmaTransformation class >> model: aRBModel pragma: aPragmaDefinition inMethod: aSelector inClass: aClassName [

	^ self new
		model: aRBModel;
		pragma: aPragmaDefinition
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : #api }
RBRemovePragmaTransformation class >> pragma: aPragmaDefinition inMethod: aSelector inClass: aClassName [

	^ self new
		pragma: aPragmaDefinition
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : #api }
RBRemovePragmaTransformation >> pragma: aPragmaDefinition inMethod: aSelector inClass: aClassName [

	self className: aClassName.
	selector := aSelector.
	pragma := aPragmaDefinition.
]

{ #category : #preconditions }
RBRemovePragmaTransformation >> preconditions [
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
						anySatisfy: [ :e | e selector = pragmaNode selector ] ]
				errorString:
					('Method named <1s> does not have a pragma named <2s>'
						expandMacrosWith: selector
						with: (self parserClass parsePragma: pragma) selector))
]

{ #category : #executing }
RBRemovePragmaTransformation >> privateTransform [
	| methodTree pragmaNode nodesToRemove |
	methodTree := self definingMethod.
	pragmaNode := self parserClass parsePragma: pragma.
	nodesToRemove := (methodTree allChildren removeDuplicates
		select: #isPragma)
		select: [ :node | node selector = pragmaNode selector ].
	nodesToRemove do: [ :node | node parent removePragma: node ].
	self definingClass compileTree: methodTree
]

{ #category : #printing }
RBRemovePragmaTransformation >> storeOn: aStream [ 

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
