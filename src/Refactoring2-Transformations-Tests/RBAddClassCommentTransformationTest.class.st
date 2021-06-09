Class {
	#name : #RBAddClassCommentTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #tests }
RBAddClassCommentTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBAddClassCommentTransformation
		comment: 'New comment for class'
		in: self changeMock name)
		transform.
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self changeMock name.
	self assert: class comment equals: 'New comment for class'
]
