Class {
	#name : #RBRemoveProtocolTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #testing }
RBRemoveProtocolTransformationTest >> testRefactoring [
	
	| refactoring |		
	refactoring := (RBAddProtocolTransformation
						protocol: 'transforming'
						inClass: #RBDummyEmptyClass)
						transform.
	RBRefactoryChangeManager instance performChange: refactoring changes.					
						
	refactoring := (RBRemoveProtocolTransformation
						protocol: 'transforming'
						inClass: #RBDummyEmptyClass)
						asRefactoring transform.
		
	self assert: refactoring model changes changes size equals: 1
]

{ #category : #testing }
RBRemoveProtocolTransformationTest >> testTransform [
	
	| transformation |
	transformation := (RBAddProtocolTransformation
							protocol: 'transforming'
							inClass: #RBDummyEmptyClass)
							transform.
		
	transformation := (RBRemoveProtocolTransformation
							model: transformation model
							protocol: 'transforming'
							inClass: #RBDummyEmptyClass)
							transform.
		
	self assert: transformation model changes changes size equals: 2
]
