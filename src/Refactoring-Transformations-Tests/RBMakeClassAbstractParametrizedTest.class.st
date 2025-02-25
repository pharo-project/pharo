Class {
	#name : 'RBMakeClassAbstractParametrizedTest',
	#superclass : 'RBWithDifferentConstructorsParametrizedTest',
	#instVars : [
		'testClass'
	],
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'tests' }
RBMakeClassAbstractParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBMakeClassAbstractTransformation };
		yourself
]

{ #category : 'running' }
RBMakeClassAbstractParametrizedTest >> setUp [
	super setUp.
	model := self rbModelForVariableTest.

	testClass := RBBasicLintRuleTestData.
	testClass class removeSelector: #isAbstract
]

{ #category : 'running' }
RBMakeClassAbstractParametrizedTest >> tearDown [

	testClass class removeSelector: #isAbstract.
	super tearDown
]

{ #category : 'tests' }
RBMakeClassAbstractParametrizedTest >> testMakeClassAbstractAddsIsAbstractMethodToClassSide [
	| refactoring |
	refactoring := rbClass class: testClass.

	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: testClass name) classSide
			parseTreeForSelector: #isAbstract)
		equals: (self parseMethod: 'isAbstract ^self == ', testClass name)
]

{ #category : 'tests' }
RBMakeClassAbstractParametrizedTest >> testMakeClassAbstractPerformChanges [
	"This test checks if actually performing refactoring applies the changes
	since model needs to be set correctly. There was a regression when model
	was nil. This test case covers that. In future we should create tests
	that check `performChanges` logic."

	| refactoring |
	refactoring := rbClass class: testClass.

	refactoring execute.
	self assert: ((refactoring model classNamed: testClass name) classSide
			parseTreeForSelector: #isAbstract)
		equals: (self parseMethod: 'isAbstract ^self == ', testClass name)
]
