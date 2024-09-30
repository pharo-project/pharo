Class {
	#name : #RBMakeClassAbstractParametrizedTest,
	#superclass : #RBWithDifferentConstructorsParametrizedTest,
	#instVars : [
		#testClass
	],
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBMakeClassAbstractParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBMakeClassAbstractTransformation };
		yourself
]

{ #category : #running }
RBMakeClassAbstractParametrizedTest >> setUp [
	super setUp.
	model := self abstractVariableTestData.

	testClass := RBBasicLintRuleTestData.
	testClass class removeSelector: #isAbstract
]

{ #category : #running }
RBMakeClassAbstractParametrizedTest >> tearDown [

	testClass class removeSelector: #isAbstract.
	super tearDown
]

{ #category : #tests }
RBMakeClassAbstractParametrizedTest >> testMakeClassAbstractAddsIsAbstractMethodToClassSide [
	| refactoring |
	refactoring := rbClass class: testClass.

	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: testClass name) classSide
			parseTreeForSelector: #isAbstract)
		equals: (self parseMethod: 'isAbstract ^self == ', testClass name)
]
