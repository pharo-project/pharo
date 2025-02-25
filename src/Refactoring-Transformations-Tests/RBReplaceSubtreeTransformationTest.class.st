Class {
	#name : 'RBReplaceSubtreeTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests' }
RBReplaceSubtreeTransformationTest >> testEmptyCode [

	self shouldFail: (RBReplaceSubtreeTransformation
			 replace: ''
			 to: '^ selector'
			 inMethod: #selector:from:
			 inClass: #RBRemoveMethodTransformation)
]

{ #category : 'tests' }
RBReplaceSubtreeTransformationTest >> testFailureExtract [

	self shouldFail: (RBReplaceSubtreeTransformation
			 replace: ':= aSelector'
			 to: '^ selector'
			 inMethod: #selector:from:
			 inClass: #RBRemoveMethodTransformation)
]

{ #category : 'tests' }
RBReplaceSubtreeTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBReplaceSubtreeTransformation
			 replace: 'selector := aSelector'
			 to: '^ selector'
			 inMethod: #selector:for:
			 inClass: #RBRemoveMethodTransformation)
]

{ #category : 'tests' }
RBReplaceSubtreeTransformationTest >> testParseFailure [

	self shouldFail: (RBReplaceSubtreeTransformation
			 replace: 'selector := aSelector'
			 to: ':= 123'
			 inMethod: #selector:from:
			 inClass: #RBRemoveMethodTransformation)
]

{ #category : 'tests' }
RBReplaceSubtreeTransformationTest >> testRefactoring [

	| transformation class |
	transformation := RBReplaceSubtreeTransformation
		                  replace: 'selector := aSelector'
		                  to: '^ selector'
		                  inMethod: #selector:from:
		                  inClass: #RBRemoveMethodTransformation.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed:
		         #RBRemoveMethodTransformation.
	self assert: (class directlyDefinesMethod: #selector:from:).
	self
		assert:
		(class parseTreeForSelector: #selector:from:) body statements size
		equals: 2.
	self assert:
		(class parseTreeForSelector: #selector:from:) body statements last
			isReturn
]

{ #category : 'tests' }
RBReplaceSubtreeTransformationTest >> testTransform [

	| transformation class |
	transformation := RBReplaceSubtreeTransformation
		                  replace: '^ 1'
		                  to: '^ 123'
		                  inMethod: #one
		                  inClass: self changeMockClass name.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: self changeMockClass name.
	self assert: (class directlyDefinesMethod: #one).
	self
		assert: (class parseTreeForSelector: #one) body statements size
		equals: 1.
	self assert:
		(class parseTreeForSelector: #one) body statements first isReturn.
	self
		assert:
		(class parseTreeForSelector: #one) body statements first value value
		equals: 123
]

{ #category : 'tests' }
RBReplaceSubtreeTransformationTest >> testTransformOneOfManyStatements [

	| transformation class |
	transformation := RBReplaceSubtreeTransformation
		                  replace:
		                  'self assert: (class parseTreeForSelector:  #selector:from:) body statements size equals: 2'
		                  to:
		                  'self assert: (class parseTreeForSelector:  #selector:from:) body statements size = 2'
		                  inMethod: #testRefactoring
		                  inClass: #RBReplaceSubtreeTransformationTest.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed:
		         #RBReplaceSubtreeTransformationTest.
	self assert: (class directlyDefinesMethod: #testRefactoring).
	self
		assert:
		(class parseTreeForSelector: #testRefactoring) body statements size
		equals: 7.
	self assert:
		((((class parseTreeForSelector: #testRefactoring) body statements
			   at: 6) sourceCode withBlanksCondensed
			  copyReplaceAll: String cr
			  with: ' ') includesSubstring:
			 'assert: (class parseTreeForSelector: #selector:from:) body statements size = 2')
]
