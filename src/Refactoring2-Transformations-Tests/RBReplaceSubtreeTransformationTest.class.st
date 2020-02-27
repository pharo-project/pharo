Class {
	#name : #RBReplaceSubtreeTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #testing }
RBReplaceSubtreeTransformationTest >> testEmptyCode [

	self shouldFail: (RBReplaceSubtreeTransformation 
							replace: ''
							to: '^ selector'
							inMethod: #selector:from:
							inClass: #RBRemoveMethodTransformation)
							asRefactoring 
]

{ #category : #testing }
RBReplaceSubtreeTransformationTest >> testExtractFailure [

	self shouldFail: (RBReplaceSubtreeTransformation 
							replace: ':= aSelector'
							to: '^ selector'
							inMethod: #selector:from:
							inClass: #RBRemoveMethodTransformation)
							asRefactoring 
]

{ #category : #testing }
RBReplaceSubtreeTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBReplaceSubtreeTransformation 
							replace: 'selector := aSelector'
							to: '^ selector'
							inMethod: #selector:for:
							inClass: #RBRemoveMethodTransformation)
							asRefactoring 
]

{ #category : #testing }
RBReplaceSubtreeTransformationTest >> testParseFailure [

	self shouldFail: (RBReplaceSubtreeTransformation 
							replace: 'selector := aSelector'
							to: ':= 123'
							inMethod: #selector:from:
							inClass: #RBRemoveMethodTransformation)
							asRefactoring 
]

{ #category : #testing }
RBReplaceSubtreeTransformationTest >> testRefactoring [

	| transformation class |
	transformation := (RBReplaceSubtreeTransformation 
							replace: 'selector := aSelector'
							to: '^ selector'
							inMethod: #selector:from:
							inClass: #RBRemoveMethodTransformation)
							asRefactoring transform. 
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: #RBRemoveMethodTransformation.
	self assert: (class directlyDefinesMethod: #selector:from:).
	self assert: (class parseTreeFor: #selector:from:) body statements size equals: 2.
	self assert: (class parseTreeFor: #selector:from:) body statements last isReturn.
]

{ #category : #testing }
RBReplaceSubtreeTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBReplaceSubtreeTransformation
							replace: '^ 1'
							to: '^ 123'
							inMethod: #one
							inClass: self changeMock name)
							transform. 
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self changeMock name asSymbol.
	self assert: (class directlyDefinesMethod: #one).
	self assert: (class parseTreeFor: #one) body statements size equals: 1.
	self assert: (class parseTreeFor: #one) body statements first isReturn.
	self assert: (class parseTreeFor: #one) body statements first value value equals: 123
]

{ #category : #testing }
RBReplaceSubtreeTransformationTest >> testTransformOneOfManyStatements [

	| transformation class |
	transformation := (RBReplaceSubtreeTransformation
		replace: 'self assert: (class parseTreeFor: #selector:from:) body statements size equals: 2'
		to: 'self assert: (class parseTreeFor: #selector:from:) body statements size = 2'
		inMethod: #testRefactoring
		inClass: #RBReplaceSubtreeTransformationTest)
		asRefactoring transform. 
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: #RBReplaceSubtreeTransformationTest.
	self assert: (class directlyDefinesMethod: #testRefactoring).
	self assert: (class parseTreeFor: #testRefactoring) body statements size equals: 6.
	self assert: ((((class parseTreeFor: #testRefactoring) body statements at: 5) sourceCode withBlanksCondensed copyReplaceAll: String cr with: ' ')
		includesSubstring: 'assert: (class parseTreeFor: #selector:from:) body statements size = 2').
]
