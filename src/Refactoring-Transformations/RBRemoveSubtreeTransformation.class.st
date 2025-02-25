"
Removes a subtree from a method. This is a first step on providing small AST tranformations. It might replace the message send, assignment, return, etc. transformations proposed by other tools. This transformation can also be reused by more complex ones, such as Extract Method.

Usage:
```
| transformation |
transformation := (RBRemoveSubtreeTransformation
		remove: 'selector := aSelector'
		fromMethod: #selector:from:
		inClass: #RBRemoveMethodTransformation)
			generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```

Preconditions:
- the class and method exist
- the code from which the subtree will be extracted should be parseable
"
Class {
	#name : 'RBRemoveSubtreeTransformation',
	#superclass : 'RBMethodTransformation',
	#instVars : [
		'sourceCode'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'api' }
RBRemoveSubtreeTransformation class >> model: aRBModel remove: aString fromMethod: aSelector inClass: aClass [

	^ self new
		model: aRBModel;
		remove: aString
		fromMethod: aSelector
		inClass: aClass;
		yourself
]

{ #category : 'api' }
RBRemoveSubtreeTransformation class >> remove: aString fromMethod: aSelector inClass: aClass [

	^ self new
		remove: aString
		fromMethod: aSelector
		inClass: aClass;
		yourself
]

{ #category : 'preconditions' }
RBRemoveSubtreeTransformation >> applicabilityPreconditions [

	^ {
		  (RBCondition definesSelector: selector in: self definingClass).
		  (RBCondition withBlock: [
			   | tree |
			   tree := self parserClass
				           parseExpression: sourceCode
				           onError: [ :string :pos |
					           self refactoringError:
						           'Invalid source to extract - ' , string ].
			   tree ifNil: [
				   self refactoringError:
					   'Invalid source to extract - ' , sourceCode ].
			   (tree isSequence and: [ tree statements isEmpty ]) ifTrue: [
				   self refactoringError: 'Selected code to extract is empty' ].
			   tree := ((self definingClass parseTreeForSelector: selector)
				            extractSubtreeWith: sourceCode) ifNil: [
				           self refactoringError:
					           'Could not extract code from method ' , selector ].
			   true ]) }
]

{ #category : 'scripting api - conditions' }
RBRemoveSubtreeTransformation >> checkPreconditions [

	self eagerlyCheckApplicabilityPreconditions 
]

{ #category : 'executing' }
RBRemoveSubtreeTransformation >> privateTransform [

	| parseTree |
	"execute in terms of nodes and not in parserewriter"
	"suggest remove similar code here"
	parseTree := self definingClass parseTreeForSelector: selector.
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

{ #category : 'api' }
RBRemoveSubtreeTransformation >> remove: aString fromMethod: aSelector inClass: aClassName [

	self className: aClassName.
	selector := aSelector.
	sourceCode := aString
]

{ #category : 'private' }
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

{ #category : 'storing' }
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
