Class {
	#name : #RBMoveClassTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #tests }
RBMoveClassTransformationTest >> testBadCategoryName [

	self shouldFail: (RBMoveClassTransformation 
							move: self class name
							to: #'Refactoring2-Transformations-Test')
							asRefactoring
]

{ #category : #tests }
RBMoveClassTransformationTest >> testRefactoring [

	| transformation class |
	self assert: self class category equals: #'Refactoring2-Transformations-Tests'.
	
	transformation := (RBMoveClassTransformation 
							move: self class name
							to: #'Refactoring2-Transformations-Utilities')
							asRefactoring transform.
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self class name. 
	self assert: class category equals: #'Refactoring2-Transformations-Utilities'
]

{ #category : #tests }
RBMoveClassTransformationTest >> testTransform [

	| transformation class |
	self assert: self changeMock category equals: #'Refactoring-Tests-Changes'.
	
	transformation := (RBMoveClassTransformation
							move: self changeMock name
							to: #'Refactoring2-Transformations-Test')
							transform.
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self changeMock name asSymbol. 
	self assert: class category equals: #'Refactoring2-Transformations-Test'
]
