Class {
	#name : 'RBRemoveClassVariableParametrizedTest',
	#superclass : 'RBWithDifferentConstructorsParametrizedTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'building suites' }
RBRemoveClassVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> ReRemoveSharedVariableRefactoring .
					  #constructor -> #variable:class: };
		addCase: { #rbClass -> RBRemoveVariableTransformation .
					  #constructor -> #classVariable:class: };
		yourself
]

{ #category : 'running' }
RBRemoveClassVariableParametrizedTest >> setUp [

	super setUp.
	model := self modelOnClasses: { RBTransformationRuleTestData . RBLintRuleTestData }
]

{ #category : 'failure tests' }
RBRemoveClassVariableParametrizedTest >> testFailureNonExistantName [

	self shouldFail: (self createRefactoringWithModel: model andArguments:
			 { #RecursiveSelfRule1. #RBTransformationRuleTestData })
]

{ #category : 'tests' }
RBRemoveClassVariableParametrizedTest >> testRemoveClassVariable [
	| refactoring class |
	refactoring :=  self createRefactoringWithModel: model andArguments:
		{'Foo1' . #RBLintRuleTestData}.
	class := refactoring model classNamed: #RBLintRuleTestData.
	self assert: (class definesClassVariable: 'Foo1').

	self executeRefactoring: refactoring.

	self deny: (class definesClassVariable: 'Foo1')
]
