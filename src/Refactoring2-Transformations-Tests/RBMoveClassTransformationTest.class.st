Class {
	#name : 'RBMoveClassTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring2-Transformations-Tests-Test',
	#package : 'Refactoring2-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests' }
RBMoveClassTransformationTest >> testBadPackageName [

	self shouldFail: (RBMoveClassTransformation move: self class name toPackage: #'Refactoring2-Transformations-Test')
]

{ #category : 'tests' }
RBMoveClassTransformationTest >> testRefactoring [

	| transformation class |
	self assert: self class category equals: #'Refactoring2-Transformations-Tests-Test'.

	transformation := (RBMoveClassTransformation move: self class name toPackage: #'Refactoring2-Transformations' inTag: #Utilities) generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: self class name.
	self assert: class category equals: #'Refactoring2-Transformations-Utilities'
]

{ #category : 'tests' }
RBMoveClassTransformationTest >> testTransform [

	| transformation class |
	self assert: self changeMockClass category equals: #'Refactoring-Tests-Changes'.

	transformation := (RBMoveClassTransformation move: self changeMockClass name toPackage: #'Refactoring2-Transformations-Tests' inTag: 'Test') generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: self changeMockClass name asSymbol.
	self assert: class category equals: #'Refactoring2-Transformations-Tests-Test'
]
