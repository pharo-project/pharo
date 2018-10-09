Class {
	#name : #RBAddProtocolTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #testing }
RBAddProtocolTransformationTest >> testTransform [
	
	| transformation |
	transformation := (RBAddProtocolTransformation new
		protocol: 'transforming'
		inClass: #RBDummyEmptyClass)
		transform.
		
	self assert: transformation model changes changes size equals: 1
]
