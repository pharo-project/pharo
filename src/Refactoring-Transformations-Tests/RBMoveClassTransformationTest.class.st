Class {
	#name : 'RBMoveClassTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests' }
RBMoveClassTransformationTest >> testBadPackageName [

	self shouldFail: (RBMoveClassTransformation move: self class name toPackage: #'Refactoring-Transformations-Test')
]

{ #category : 'tests' }
RBMoveClassTransformationTest >> testRefactoring [

	| transformation class |
	self
		assert: self class package name
		equals: #'Refactoring-Transformations-Tests'.
	self assert: self class packageTag name equals: #Test.

	transformation := RBMoveClassTransformation
		                  move: self class name
		                  toPackage: #'Refactoring-Transformations'
		                  inTag: #Utilities.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: self class name.
	self assert: class packageName equals: #'Refactoring-Transformations'.
	self assert: class tagName equals: #Utilities
]

{ #category : 'tests' }
RBMoveClassTransformationTest >> testTransform [

	| transformation class |
	self
		assert: self changeMockClass package name
		equals: #'Refactoring-Changes-Tests'.
	self assert: self changeMockClass packageTag isRoot.

	transformation := RBMoveClassTransformation
		                  move: self changeMockClass name
		                  toPackage: #'Refactoring-Transformations-Tests'
		                  inTag: 'Test'.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: self changeMockClass name.
	self
		assert: class packageName
		equals: #'Refactoring-Transformations-Tests'.
	self assert: class tagName equals: #Test
]
