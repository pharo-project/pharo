"
Adds a subtree inside a method. It is required an interval indicating where the subtree shall be added. If the interval is invalid, the new subtree will be added at the end of the mehtod body (or before the return node, if it exists).

Usage:
| transformation |
transformation := (RBAddSubtreeTransformation
		interval: (0 to: 1)
		with: '^ selector'
		from: #selector:from:
		in: #RBRemoveMethodTransformation)
		transform. 
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
- the class and method exist
- the code from which the subtree will be extracted is parseable
"
Class {
	#name : 'RBAddSubtreeTransformation',
	#superclass : 'RBMethodTransformation',
	#instVars : [
		'interval',
		'sourceCode'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'api' }
RBAddSubtreeTransformation class >> interval: anInterval with: aString from: aSelector in: aClass [

	^ self new
		interval: anInterval
		with: aString
		from: aSelector
		in: aClass;
		yourself
]

{ #category : 'api' }
RBAddSubtreeTransformation class >> model: aRBModel interval: anInterval with: aString from: aSelector in: aClass [

	^ self new
		model: aRBModel;
		interval: anInterval
		with: aString
		from: aSelector
		in: aClass;
		yourself
]

{ #category : 'utilities' }
RBAddSubtreeTransformation >> addNode: aNode toSequence: aSequenceNode [
	"Searches for the statement before which the new code should be added.
	 If not found, it just adds the new code at the end of the block"

	aSequenceNode statements
		detect: [ :node | node intersectsInterval: interval ]
		ifFound: [ :node |
						node isBlock "special case when a block is empty"
							ifTrue: [ node body addNode: aNode ]
							ifFalse: [ aSequenceNode addNode: aNode before: node ]].
	^ aSequenceNode methodNode
]

{ #category : 'preconditions' }
RBAddSubtreeTransformation >> applicabilityPreconditions [

	^ {
		  (RBCondition definesSelector: selector in: self definingClass).
		  (RBCondition withBlock: [
			   self parserClass
				   parseExpression: sourceCode
				   onError: [ :string :pos |
					   self refactoringError: 'Invalid source to add - ' , string ].
			   true ]) }
]

{ #category : 'api' }
RBAddSubtreeTransformation >> interval: anInterval with: aString from: aSelector in: aClass [

	self className: aClass.
	selector := aSelector.
	interval := anInterval.
	sourceCode := aString
]

{ #category : 'executing' }
RBAddSubtreeTransformation >> privateTransform [
	| parseTree newNode |
	parseTree := self definingClass parseTreeForSelector: selector.
	parseTree ifNil: [ ^ self ].
	newNode := self parserClass
		parseExpression: sourceCode
		onError: [ :string :pos | ^ self ].

	"The method is empty, then just add the new code"
	parseTree body statements
		ifEmpty: [ parseTree addNode: newNode.
			^ self ].

	"It tries to find the block to insert new code.
	 If not found, e.g., the interval is invalid, it adds the code at the end."
	parseTree := parseTree allSequenceNodes
		detect: [ :sequence | sequence intersectsInterval: interval ]
		ifFound: [ :sequence | self addNode: newNode toSequence: sequence ]
		ifNone: [ parseTree body addNodeLast: newNode.
			parseTree ].
	self definingClass compileTree: parseTree
]

{ #category : 'storing' }
RBAddSubtreeTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' interval: ''';
		nextPutAll: interval asString;
		nextPutAll: ' with: ''';
		nextPutAll: sourceCode;
		nextPutAll: ''' from: ''';
		nextPutAll: selector;
		nextPutAll: ''' in: '.
	class storeOn: aStream.
	aStream nextPut: $)
]
