"
Replaces a piece of code by another in a method. Internally, this transformation replaces the corresponding subtrees, so they have to be syntactically correct.

Usage:
| transformation |
transformation := (RBReplaceSubtreeTransformation
		replace: 'selector := aSelector'
		to: '^ selector'
		inMethod: #selector:from:
		inClass: #RBRemoveMethodTransformation)
		transform.
(ChangesBrowser changes: transformation model changes changes) open
"
Class {
	#name : #RBReplaceSubtreeTransformation,
	#superclass : #RBMethodTransformation,
	#instVars : [
		'oldSourceCode',
		'newSourceCode'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBReplaceSubtreeTransformation class >> model: aRBModel replace: oldSourceCode to: newSourceCode inMethod: aSelector inClass: aClass [

	^ self new
		model: aRBModel;
		replace: oldSourceCode
		to: newSourceCode
		inMethod: aSelector
		inClass: aClass;
		yourself 
]

{ #category : #api }
RBReplaceSubtreeTransformation class >> replace: oldSourceCode to: newSourceCode inMethod: aSelector inClass: aClass [

	^ self new
		replace: oldSourceCode
		to: newSourceCode
		inMethod: aSelector
		inClass: aClass;
		yourself 
]

{ #category : #preconditions }
RBReplaceSubtreeTransformation >> preconditions [
	^ (RBCondition definesSelector: selector in: self definingClass)
		&
			(RBCondition
				withBlock: [ | tree |
					tree := self parserClass
						parseExpression: oldSourceCode
						onError:
							[ :string :pos | self refactoringError: 'Invalid source to extract - ' , string ].
					tree
						ifNil: [ self
								refactoringError: 'Invalid source to extract - ' , oldSourceCode ].
					(tree isSequence and: [ tree statements isEmpty ])
						ifTrue: [ self refactoringError: 'Selected code to extract is empty' ].
					tree := ((self definingClass parseTreeFor: selector)
						extractSubtreeWith: oldSourceCode)
						ifNil: [ self
								refactoringError: 'Could not extract code from method ' , selector ].
					true ])
		&
			(RBCondition
				withBlock: [ self parserClass
						parseExpression: newSourceCode
						onError:
							[ :string :pos | self refactoringError: 'Invalid source to add - ' , string ].
					true ])
]

{ #category : #executing }
RBReplaceSubtreeTransformation >> privateTransform [
	| tree oldTree newTree |
	tree := self definingClass parseTreeFor: selector.
	tree ifNil: [ ^ self ].
	oldTree := tree extractSubtreeWith: oldSourceCode.
	oldTree ifNil: [ ^ self ].
	newTree := self parserClass
		parseExpression: newSourceCode
		onError: [ :string :pos | ^ nil ].
	newTree ifNil: [ ^ self ].
	tree := oldTree isSequence
		ifTrue: [ self parseTreeRewriterClass
				replaceStatements: oldTree formattedCode
				with: newSourceCode
				in: tree ]
		ifFalse: [ self parseTreeRewriterClass
				replace: oldTree formattedCode
				with: newSourceCode
				in: tree
				onInterval: tree sourceInterval ].
	self definingClass compileTree: tree
]

{ #category : #api }
RBReplaceSubtreeTransformation >> replace: anOldString to: aNewString inMethod: aSelector inClass: aClass [

	self className: aClass.
	selector := aSelector.
	oldSourceCode := anOldString.
	newSourceCode := aNewString
]

{ #category : #printing }
RBReplaceSubtreeTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' replace: ''';
		nextPutAll: oldSourceCode;
		nextPutAll: ''' to: ''';
		nextPutAll: newSourceCode;
		nextPutAll: ''' inMethod: #';
		nextPutAll: selector;
		nextPutAll: ' inClass: '.
	class storeOn: aStream.
	aStream nextPut: $)
]
