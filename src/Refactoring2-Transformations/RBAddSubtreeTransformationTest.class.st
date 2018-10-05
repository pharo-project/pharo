Class {
	#name : #RBAddSubtreeTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #testing }
RBAddSubtreeTransformationTest >> testMethodDoesNotExist [

	| transformation |
	transformation := (RBAddSubtreeTransformation
		interval: (0 to: 1)
		with: 'self printString'
		from: #two
		in: self changeMock name)
		transform. 
	
	self assert: transformation model changes changes isEmpty
]

{ #category : #testing }
RBAddSubtreeTransformationTest >> testParseFailure [

	| transformation |
	transformation := (RBAddSubtreeTransformation
		interval: (0 to: 1)
		with: ':= 123'
		from: #one
		in: self changeMock name)
		transform. 
	
	self assert: transformation model changes changes isEmpty
]

{ #category : #testing }
RBAddSubtreeTransformationTest >> testRefactoring [

	self 
		shouldFail: (RBAddSubtreeTransformation
							interval: (0 to: 1)
							with: ':= 123'
							from: #selector:from:
							in: #RBRemoveMethodTransformation)
							asRefactoring;
		shouldFail: (RBAddSubtreeTransformation
							interval: (0 to: 1)
							with: '^ selector'
							from: #selector:for:
							in: #RBRemoveMethodTransformation)
							asRefactoring
]

{ #category : #testing }
RBAddSubtreeTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBAddSubtreeTransformation
		interval: (0 to: 1)
		with: 'self printString'
		from: #one
		in: self changeMock name)
		transform. 
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self changeMock name asSymbol.
	self assert: (class directlyDefinesMethod: #one).
	self assert: (class parseTreeFor: #one) body statements size equals: 2
]
