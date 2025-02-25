Class {
	#name : 'RBMergeInstanceVariableIntoAnotherParametrizedTest',
	#superclass : 'RBWithDifferentConstructorsParametrizedTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'building suites' }
RBMergeInstanceVariableIntoAnotherParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBMergeInstanceVariableIntoAnother .
					  #constructor -> #rename:to:in: };
		addCase: { #rbClass -> RBMergeInstanceVariableIntoAnotherTransformation .
					  #constructor -> #renameInstanceVariable:to:in: };
		yourself
]

{ #category : 'running' }
RBMergeInstanceVariableIntoAnotherParametrizedTest >> setUp [
	super setUp.
	model := self modelOnClasses: { RBLintRuleTestData . RBBasicLintRuleTestData }.
]

{ #category : 'failure tests' }
RBMergeInstanceVariableIntoAnotherParametrizedTest >> testFailureEqualsVariables [

	self shouldFail: (self createRefactoringWithModel: model andArguments: {
				 'classBlock'.
				 'classBlock'.
				 RBBasicLintRuleTestData })
]

{ #category : 'failure tests' }
RBMergeInstanceVariableIntoAnotherParametrizedTest >> testFailureNonExistingVariable [

	self shouldFail: (self createRefactoringWithModel: model andArguments: {
				 'classBlock'.
				 'anInstVar1'.
				 RBBasicLintRuleTestData })
]

{ #category : 'tests' }
RBMergeInstanceVariableIntoAnotherParametrizedTest >> testReplaceVariableIntoAnother [
	| refactoring class |
	refactoring := self createRefactoringWithModel: model andArguments:
		{ 'classBlock' . 'anInstVar' . RBBasicLintRuleTestData }.
	class := refactoring model classNamed: #RBBasicLintRuleTestData.
	self assert: (class whichSelectorsReferToInstanceVariable: 'classBlock') isNotEmpty.
	self executeRefactoring: refactoring.

	self assert: (class whichSelectorsReferToInstanceVariable: 'classBlock') isEmpty
]

{ #category : 'tests' }
RBMergeInstanceVariableIntoAnotherParametrizedTest >> testReplaceVariableIntoSupeclassVariable [
	| refactoring class |
	refactoring := self createRefactoringWithModel: model andArguments:
		{ 'name' . 'classBlock' . RBBasicLintRuleTestData }.
	class := refactoring model classNamed: #RBBasicLintRuleTestData.
	self assert: (class whichSelectorsReferToInstanceVariable: 'name') isNotEmpty.
	self executeRefactoring: refactoring.
	self assert: (class definesInstanceVariable: 'name').
	self assert: (class directlyDefinesInstanceVariable: 'classBlock').

	self assert: (class whichSelectorsReferToInstanceVariable: 'name') isEmpty
]
