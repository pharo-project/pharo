Class {
	#name : #RBAddMethodCommentTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #tests }
RBAddMethodCommentTransformationTest >> testTransform [

	| transformation method |
	transformation := (RBAddMethodCommentTransformation
		comment: 'New comment for method'
		inMethod: #one
		inClass: self changeMock name)
		transform.
	
	self assert: transformation model changes changes size equals: 1.
	
	method := (transformation model classNamed: self changeMock name) methodFor: #one.
	self assert: method ast comments first contents equals: 'New comment for method'
]
