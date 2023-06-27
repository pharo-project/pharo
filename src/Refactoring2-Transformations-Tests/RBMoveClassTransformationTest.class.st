Class {
	#name : #RBMoveClassTransformationTest,
	#superclass : #RBAbstractTransformationTest,
	#category : #'Refactoring2-Transformations-Tests-Test'
}

{ #category : #tests }
RBMoveClassTransformationTest >> testBadCategoryName [

	self shouldFail: (RBMoveClassTransformation
			 move: self class name
			 to: #'Refactoring2-Transformations-Test')
]

{ #category : #tests }
RBMoveClassTransformationTest >> testRefactoring [

	| transformation class |
	self
		assert: self class category
		equals: #'Refactoring2-Transformations-Tests-Test'.

	transformation := (RBMoveClassTransformation
		                   move: self class name
		                   to: #'Refactoring2-Transformations-Utilities')
		                  transform.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: self class name.
	self
		assert: class category
		equals: #'Refactoring2-Transformations-Utilities'
]

{ #category : #tests }
RBMoveClassTransformationTest >> testTransform [

	| transformation class |
	self assert: self changeMockClass category equals: #'Refactoring-Tests-Changes'.

	transformation := (RBMoveClassTransformation
							move: self changeMockClass name
							to: #'Refactoring2-Transformations-Test')
							transform.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: self changeMockClass name asSymbol.
	self assert: class category equals: #'Refactoring2-Transformations-Test'
]
