Class {
	#name : #RBRemoveProtocolTransformationTest,
	#superclass : #RBAbstractTransformationTest,
	#category : #'Refactoring2-Transformations-Tests-Test'
}

{ #category : #tests }
RBRemoveProtocolTransformationTest >> testRefactoring [

	| refactoring |
	refactoring := (RBAddProtocolTransformation
		                protocol: 'transforming'
		                inClass: #RBDummyEmptyClass) primitiveExecute.
	RBRefactoryChangeManager instance performChange: refactoring changes.

	refactoring := (RBRemoveProtocolTransformation
		                protocol: 'transforming'
		                inClass: #RBDummyEmptyClass) primitiveExecute.

	self assert: refactoring model changes changes size equals: 1
]

{ #category : #tests }
RBRemoveProtocolTransformationTest >> testTransform [

	| transformation |
	transformation := (RBAddProtocolTransformation
		                   protocol: 'transforming'
		                   inClass: #RBDummyEmptyClass) primitiveExecute.

	transformation := (RBRemoveProtocolTransformation
		                   model: transformation model
		                   protocol: 'transforming'
		                   inClass: #RBDummyEmptyClass) primitiveExecute.

	self assert: transformation model changes changes size equals: 2
]
