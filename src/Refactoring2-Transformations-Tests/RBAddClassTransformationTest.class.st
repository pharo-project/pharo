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

{ #category : #testing }
RBAddClassTransformationTest >> testAddClass [
	| refactoring newClass superClass classTest |
	refactoring := (RBAddClassTransformation
		addClass: #FooTest
		superclass: #RBTransformationTest
		subclasses: #(RBAddClassTransformationTest)
		category: #'Refactory-Testing') asRefactoring.
	refactoring transform.

	newClass := refactoring model classNamed: #FooTest.
	superClass := refactoring model classNamed: #RBTransformationTest.
	classTest := refactoring model classNamed: self class name.
	self assert: newClass superclass equals: superClass.
	self assert: (superClass subclasses includes: newClass).
	self assert: newClass theMetaClass superclass equals: superClass theMetaClass.
	self assert: (superClass theMetaClass subclasses includes: newClass theMetaClass).
	self assert: classTest superclass equals: newClass.
	self assert: (newClass subclasses includes: classTest).
	self assert: classTest theMetaClass superclass equals: newClass theMetaClass.
	self assert: (newClass theMetaClass subclasses includes: classTest theMetaClass)
]

{ #category : #testing }
RBAddClassTransformationTest >> testExistingName [

	self shouldFail: (
		RBAddClassTransformation
			addClass: #Object
			superclass: self class name
			subclasses: #()
			category: #'Refactory-Testing' )
			asRefactoring
]

{ #category : #testing }
RBAddClassTransformationTest >> testInvalidSubclass [

	self shouldFail: (
		RBAddClassTransformation
			addClass: #Foo
			superclass: #RBCompositeLintRuleTestData
			subclasses: (Array with: RBBasicLintRuleTestData)
			category: #'Refactory-Tesing')
			asRefactoring
]

{ #category : #testing }
RBAddClassTransformationTest >> testModelAddClass [
	| refactoring newClass superClass subclass |
	subclass := model classNamed: #Bar.
	superClass := model classNamed: #Foo.
	refactoring := (RBAddClassTransformation
		model: model
		addClass: #FooTest
		superclass: #Foo
		subclasses: #(Bar)
		category: #'Refactory-Testing') asRefactoring.
	refactoring transform.

	newClass := model classNamed: #FooTest.
	self assert: newClass superclass equals: superClass.
	self assert: (superClass subclasses includes: newClass).
	self assert: newClass theMetaClass superclass equals: superClass theMetaClass.
	self assert: (superClass theMetaClass subclasses includes: newClass theMetaClass).
	self assert: subclass superclass equals: newClass.
	self assert: (newClass subclasses includes: subclass).
	self assert: subclass theMetaClass superclass equals: newClass theMetaClass.
	self assert: (newClass theMetaClass subclasses includes: subclass theMetaClass)
]

{ #category : #testing }
RBAddClassTransformationTest >> testModelExistingName [

	self shouldFail: (
		RBAddClassTransformation
			model: model
			addClass: #Foo
			superclass: #Object
			subclasses: #()
			category: #'Refactory-Testing')
			asRefactoring
]

{ #category : #testing }
RBAddClassTransformationTest >> testModelInvalidSubclass [

	self shouldFail: (
		RBAddClassTransformation
			model: model
			addClass: #Foo2
			superclass: #Object
			subclasses: (Array with: (model classNamed: #Bar))
			category: #'Refactory-Tesing')
			asRefactoring
]

{ #category : #testing }
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
