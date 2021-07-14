Class {
	#name : #RBAddClassTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #running }
RBAddClassTransformationTest >> setUp [

	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #tests }
RBAddClassTransformationTest >> testTransform [
	| transformation newClass superclass |
	transformation := (RBAddClassTransformation
		addClass: self changeMock name , 'Temporary'
		superclass: #Object
		subclasses: OrderedCollection new
		category: self class category) transform.

	self assert: transformation model changes changes size equals: 1.

	newClass := transformation model classNamed: (self changeMock name , 'Temporary') asSymbol.
	superclass := transformation model classNamed: #Object.
	self assert: newClass superclass equals: superclass.
	self assert: (superclass subclasses includes: newClass).
	self assert: newClass theMetaClass superclass equals: superclass theMetaClass.
	self assert: (superclass theMetaClass subclasses includes: newClass theMetaClass)
]
