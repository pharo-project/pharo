Class {
	#name : 'RBRemoveProtocolTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests' }
RBRemoveProtocolTransformationTest >> testRefactoring [

	| refactoring |
	refactoring := (RBAddProtocolTransformation
		                protocol: 'transforming'
		                inClass: #RBDummyEmptyClass).
	refactoring generateChanges.
	refactoring performChanges.

	refactoring := (RBRemoveProtocolTransformation
		                protocol: 'transforming'
		                inClass: #RBDummyEmptyClass).
	refactoring generateChanges.

	self assert: refactoring model changes changes size equals: 1
]

{ #category : 'tests' }
RBRemoveProtocolTransformationTest >> testTransform [

	| transformation |
	transformation := (RBAddProtocolTransformation
		                   protocol: 'transforming'
		                   inClass: #RBDummyEmptyClass).
	transformation execute.

	transformation := (RBRemoveProtocolTransformation
		                   model: transformation model
		                   protocol: 'transforming'
		                   inClass: #RBDummyEmptyClass).
	transformation execute.

	self assert: transformation model changes changes size equals: 2
]
