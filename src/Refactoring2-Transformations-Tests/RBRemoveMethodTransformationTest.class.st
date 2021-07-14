Class {
	#name : #RBRemoveMethodTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #tests }
RBRemoveMethodTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBRemoveMethodTransformation 
		selector: #one
		from: self changeMock name)
		transform. 
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self changeMock name asSymbol.
	self deny: (class directlyDefinesMethod: #one).
]
