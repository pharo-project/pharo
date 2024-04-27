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
		                inClass: #RBDummyEmptyClass) generateChanges.
	refactoring performChanges.

	refactoring := (RBRemoveProtocolTransformation
		                protocol: 'transforming'
		                inClass: #RBDummyEmptyClass) generateChanges.

	self assert: refactoring model changes changes size equals: 1
]

{ #category : 'tests' }
RBRemoveProtocolTransformationTest >> testTransform [

	| transformation |
	transformation := (RBAddProtocolTransformation
		                   protocol: 'transforming'
		                   inClass: #RBDummyEmptyClass) generateChanges.

	transformation := (RBRemoveProtocolTransformation
		                   model: transformation model
		                   protocol: 'transforming'
		                   inClass: #RBDummyEmptyClass) generateChanges.

	self assert: transformation model changes changes size equals: 2
]
