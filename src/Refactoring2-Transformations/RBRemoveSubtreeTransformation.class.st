"
Removes a subtree from a method. This is a first step on providing small AST tranformations. It might replace the message send, assignment, return, etc. transformations proposed by other tools. This transformation can also be reused by more complex ones, such as Extract Method.

Usage:
| transformation |
transformation := (RBRemoveSubtreeTransformation
		code: 'selector := aSelector'
		from: #selector:from:
		in: #RBRemoveMethodTransformation)
		transform. 
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
- the class and method exist
- the code from which the subtree will be extracted should be parseable
"
Class {
	#name : #RBRemoveSubtreeTransformation,
	#superclass : #RBMethodTransformation,
	#instVars : [
		'sourceCode'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBRemoveSubtreeTransformation class >> code: aString from: aSelector in: aClass [

	^ self new
		code: aString
		from: aSelector
		in: aClass;
		yourself
]

{ #category : #api }
RBRemoveSubtreeTransformation class >> model: aRBModel code: aString from: aSelector in: aClass [

	^ self new
		model: aRBModel;
		code: aString
		from: aSelector
		in: aClass;
		yourself
]

{ #category : #api }
RBRemoveSubtreeTransformation >> code: aString from: aSelector in: aClassName [

	self className: aClassName.
	selector := aSelector.
	sourceCode := aString
]

{ #category : #preconditions }
RBRemoveSubtreeTransformation >> preconditions [
	^ (RBCondition definesSelector: selector in: self definingClass)
		&
			(RBCondition
				withBlock: [ | tree |
					tree := self parserClass
						parseExpression: sourceCode
						onError:
							[ :string :pos | self refactoringError: 'Invalid source to extract - ' , string ].
					tree
						ifNil: [ self refactoringError: 'Invalid source to extract - ' , sourceCode ].
					(tree isSequence and: [ tree statements isEmpty ])
						ifTrue: [ self refactoringError: 'Selected code to extract is empty' ].
					tree := ((self definingClass parseTreeFor: selector)
						extractSubtreeWith: sourceCode)
						ifNil: [ self
								refactoringError: 'Could not extract code from method ' , selector ].
					true ])
]

{ #category : #executing }
RBRemoveSubtreeTransformation >> privateTransform [

	| parseTree |
	"execute in terms of nodes and not in parserewriter"
	"suggest remove similar code here"
	parseTree := self definingClass parseTreeFor: selector.
	parseTree ifNil: [ ^ self ].
	
	"Halt now."
	(parseTree extractSubtreeWith: sourceCode)
		ifNotNil: [ :subtree |
			parseTree removeSubtree: subtree.
			self definingClass compileTree: parseTree ]
		ifNil: [ ^ self ].
	
	
	"(subtree := parseTree extractSubtreeWith: sourceCode) ifNil: [ ^ self ].	
	parseTree := parseTree removeSubtree: subtree.
	self definingClass compileTree: parseTree"
]

{ #category : #private }
RBRemoveSubtreeTransformation >> removeSubtree: aTree in: aMethodTree [
	^ [ self parseTreeRewriterClass
		replaceStatements: aTree formattedCode
		with: ''
		in: aMethodTree
		onInterval: aTree sourceInterval ]
		on: Error
		do: [ self parseTreeRewriterClass
				replace: aTree formattedCode
				with: ''
				in: aMethodTree
				onInterval: aTree sourceInterval ]
]

{ #category : #printing }
RBRemoveSubtreeTransformation >> storeOn: aStream [ 

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' code: ''';
		nextPutAll: sourceCode;
		nextPutAll: ''' from: ''';
		nextPutAll: selector;
		nextPutAll: ''' in: '.
	class storeOn: aStream.
	aStream nextPut: $)
]
