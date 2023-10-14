Class {
	#name : 'RBMoveClassTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests' }
RBMoveClassTransformationTest >> testBadPackageName [

	self shouldFail: (RBMoveClassTransformation move: self class name toPackage: #'Refactoring2-Transformations-Test')
]

{ #category : 'tests' }
RBMoveClassTransformationTest >> testRefactoring [

	| transformation class |
	self assert: self class package name equals: #'Refactoring2-Transformations-Tests'.
	self assert: self class packageTag name equals: #Test.

	transformation := (RBMoveClassTransformation move: self class name toPackage: #'Refactoring2-Transformations' inTag: #Utilities) generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: self class name.
	self assert: class packageName equals: #'Refactoring2-Transformations'.
	self assert: class tagName equals: #Utilities
]

{ #category : 'tests' }
RBMoveClassTransformationTest >> testTransform [

	| transformation class |
	self assert: self changeMockClass package name equals: #'Refactoring-Tests-Changes'.
	self assert: self changeMockClass packageTag isRoot.

	transformation := (RBMoveClassTransformation move: self changeMockClass name toPackage: #'Refactoring2-Transformations-Tests' inTag: 'Test') generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: self changeMockClass name.
	self assert: class packageName equals: #'Refactoring2-Transformations-Tests'.
	self assert: class tagName equals: #Test
]
