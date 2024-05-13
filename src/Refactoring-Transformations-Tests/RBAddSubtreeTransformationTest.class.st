Class {
	#name : 'RBAddSubtreeTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests' }
RBAddSubtreeTransformationTest >> testMethodDoesNotExist [
	
	self shouldFail: (RBAddSubtreeTransformation
		interval: (0 to: 1)
		with: 'self printString'
		from: #two
		in: self changeMockClass name)
]

{ #category : 'tests' }
RBAddSubtreeTransformationTest >> testParseFailure [

	self shouldFail: (RBAddSubtreeTransformation
		interval: (0 to: 1)
		with: ':= 123'
		from: #one
		in: self changeMockClass name)
]

{ #category : 'tests' }
RBAddSubtreeTransformationTest >> testRefactoring [

	self
		shouldFail: (RBAddSubtreeTransformation
				 interval: (0 to: 1)
				 with: ':= 123'
				 from: #selector:from:
				 in: #RBRemoveMethodTransformation);
		shouldFail: (RBAddSubtreeTransformation
				 interval: (0 to: 1)
				 with: '^ selector'
				 from: #selector:for:
				 in: #RBRemoveMethodTransformation)
]

{ #category : 'tests' }
RBAddSubtreeTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBAddSubtreeTransformation
		                   interval: (0 to: 1)
		                   with: 'self printString'
		                   from: #one
		                   in: self changeMockClass name).
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: self changeMockClass name.
	self assert: (class directlyDefinesMethod: #one).
	self
		assert: (class parseTreeForSelector: #one) body statements size
		equals: 2
]
