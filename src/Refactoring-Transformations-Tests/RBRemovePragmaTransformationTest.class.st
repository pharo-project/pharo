Class {
	#name : 'RBRemovePragmaTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'utilities' }
RBRemovePragmaTransformationTest >> methodAfter [

	| variable |
	variable := 'String'.
	variable byteAt: 1
]

{ #category : 'utilities' }
RBRemovePragmaTransformationTest >> methodBefore [
	<pragmaForTesting: 34>

	| variable |
	variable := 'String'.
	variable byteAt: 1
]

{ #category : 'tests' }
RBRemovePragmaTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBRemovePragmaTransformation
			 pragma: '<pragmaForTesting: 34>'
			 inMethod: #methodBefore
			 inClass: #RBPragmaTransformationTest)
]

{ #category : 'tests' }
RBRemovePragmaTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBRemovePragmaTransformation
			 pragma: '<pragmaForTesting: 34>'
			 inMethod: #method
			 inClass: #RBRemovePragmaTransformationTest)
]

{ #category : 'tests' }
RBRemovePragmaTransformationTest >> testPragmaDoesNotExist [

	self shouldFail: (RBRemovePragmaTransformation
			 pragma: '<gtPresentationOrder: 34>'
			 inMethod: #methodBefore
			 inClass: #RBRemovePragmaTransformationTest)
]

{ #category : 'tests' }
RBRemovePragmaTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := RBRemovePragmaTransformation
		               pragma: '<pragmaForTesting: 34>'
		               inMethod: #methodBefore
		               inClass: #RBRemovePragmaTransformationTest.
	refactoring generateChanges.

	self assert: refactoring model changes changes size equals: 1.

	class := refactoring model classNamed:
		         #RBRemovePragmaTransformationTest.
	self
		assert: (class parseTreeForSelector: #methodBefore) body
		equals: (class parseTreeForSelector: #methodAfter) body
]

{ #category : 'tests' }
RBRemovePragmaTransformationTest >> testTransform [

	| transformation class |
	transformation := RBRemovePragmaTransformation new
		                  pragma: '<pragmaForTesting: 34>'
		                  inMethod: #methodBefore
		                  inClass: self class name.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: self class name.
	self
		assert: (class parseTreeForSelector: #methodBefore) body
		equals: (class parseTreeForSelector: #methodAfter) body
]
