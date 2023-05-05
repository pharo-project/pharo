Class {
	#name : #RBRemoveInstanceVariableParametrizedTest,
	#superclass : #RBWithDifferentConstructorsParametrizedTest,
	#category : #'Refactoring2-Transformations-Tests-SingleParametrized'
}

{ #category : #tests }
RBRemoveInstanceVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBRemoveInstanceVariableRefactoring .
					  #constructor -> #variable:class: };
		yourself
]

{ #category : #tests }
RBRemoveInstanceVariableParametrizedTest >> testModelRemoveInstanceVariable [
	| class |
	model defineClass: 'nil << #Object
		slots: {#foo1};
		package: #''Refactory-Test data'''.

	class := model classNamed: #Object.
	self assert: (class definesInstanceVariable: 'foo1').

	self executeRefactoring: (self createRefactoringWithModel: model
		andArguments: {'foo1' . class name asSymbol }).

	self deny: (class definesInstanceVariable: 'foo1')
]

{ #category : #'failure tests' }
RBRemoveInstanceVariableParametrizedTest >> testNonExistantName [
	self shouldFail: (self createRefactoringWithArguments:
		{ 'name1' . #RBLintRuleTestData })
]

{ #category : #'failure tests' }
RBRemoveInstanceVariableParametrizedTest >> testReferencedVariable [
	self shouldWarn: (self createRefactoringWithArguments:
		{ 'name' . #RBLintRuleTestData })
]

{ #category : #tests }
RBRemoveInstanceVariableParametrizedTest >> testRemoveInstVar [
	| refactoring class |
	refactoring := self createRefactoringWithArguments:
		{'foo1' . #RBLintRuleTestData}.
	class := refactoring model classNamed: #RBLintRuleTestData.
	self assert: (class definesInstanceVariable: 'foo1').

	self executeRefactoring: refactoring.

	self deny: (class definesInstanceVariable: 'foo1')
]
