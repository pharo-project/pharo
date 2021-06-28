Class {
	#name : #RBAddClassTransformationTest1,
	#superclass : #RBParametrizedTransformationTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #'failure tests' }
RBAddClassTransformationTest1 >> setUp [

	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #'failure tests' }
RBAddClassTransformationTest1 >> testExistingName [

	self shouldFail: (
		RBAddClassTransformation
			addClass: #Object
			superclass: self class name
			subclasses: #()
			category: #'Refactory-Testing' )
			asRefactoring
]

{ #category : #'failure tests' }
RBAddClassTransformationTest1 >> testInvalidSubclass [

	self shouldFail: (
		RBAddClassTransformation
			addClass: #Foo
			superclass: #RBCompositeLintRuleTestData
			subclasses: (Array with: RBBasicLintRuleTestData)
			category: #'Refactory-Tesing')
			asRefactoring
]

{ #category : #'failure tests' }
RBAddClassTransformationTest1 >> testModelExistingName [

	self shouldFail: (
		RBAddClassTransformation
			model: model
			addClass: #Foo
			superclass: #Object
			subclasses: #()
			category: #'Refactory-Testing')
			asRefactoring
]

{ #category : #'failure tests' }
RBAddClassTransformationTest1 >> testModelInvalidSubclass [

	self shouldFail: (
		RBAddClassTransformation
			model: model
			addClass: #Foo2
			superclass: #Object
			subclasses: (Array with: (model classNamed: #Bar))
			category: #'Refactory-Tesing')
			asRefactoring
]
