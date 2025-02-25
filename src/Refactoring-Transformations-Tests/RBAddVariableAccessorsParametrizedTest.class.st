Class {
	#name : 'RBAddVariableAccessorsParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
RBAddVariableAccessorsParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> ReCreateAccessorsForVariableTransformation };
		addCase: { #rbClass -> RBAddVariableAccessorTransformation };
		yourself
]

{ #category : 'running' }
RBAddVariableAccessorsParametrizedTest >> setUp [
	super setUp.
	model := self rbModelForVariableTest
]

{ #category : 'tests' }
RBAddVariableAccessorsParametrizedTest >> testExistingClassVariableAccessors [

	| refactoring |
	refactoring := rbClass
		               classVariable: 'Name1'
		               class: #RBLintRuleTestData.

	self executeRefactoring: refactoring.

	self assertEmpty: refactoring changes changes
]

{ #category : 'tests' }
RBAddVariableAccessorsParametrizedTest >> testExistingInstanceVariableAccessors [

	| refactoring |
	refactoring := rbClass
		               instanceVariable: 'name'
		               class: #RBLintRuleTestData.

	self executeRefactoring: refactoring.

	self assertEmpty: refactoring changes changes
]

{ #category : 'failure tests' }
RBAddVariableAccessorsParametrizedTest >> testFailureNonExistantClassVariable [

	self shouldFail:
		(rbClass classVariable: 'Foo' class: #RBBasicLintRuleTestData)
]

{ #category : 'failure tests' }
RBAddVariableAccessorsParametrizedTest >> testFailureNonExistantInstanceVariable [

	self shouldFail:
		(rbClass instanceVariable: 'foo' class: #RBBasicLintRuleTestData)
]

{ #category : 'tests' }
RBAddVariableAccessorsParametrizedTest >> testNewClassVariableAccessors [

	| refactoring class |
	refactoring := rbClass
		               classVariable: 'Foo1'
		               class: #RBLintRuleTestData.

	self executeRefactoring: refactoring.

	class := refactoring model metaclassNamed: #RBLintRuleTestData.
	self denyEmpty: refactoring changes changes.
	self
		assert: (class parseTreeForSelector: #foo1)
		equals: (self parseMethod: 'foo1 ^Foo1').
	self
		assert: (class parseTreeForSelector: #foo1:)
		equals: (self parseMethod: 'foo1: anObject Foo1 := anObject')
]

{ #category : 'tests' }
RBAddVariableAccessorsParametrizedTest >> testNewInstanceVariableAccessors [

	| refactoring class |
	refactoring := rbClass
		               instanceVariable: 'foo1'
		               class: #RBLintRuleTestData.

	self executeRefactoring: refactoring.

	class := refactoring model classNamed: #RBLintRuleTestData.
	self denyEmpty: refactoring changes changes.
	self
		assert: (class parseTreeForSelector: #foo1)
		equals: (self parseMethod: 'foo1 ^foo1').
	self
		assert: (class parseTreeForSelector: #foo1:)
		equals: (self parseMethod: 'foo1: anObject foo1 := anObject')
]
