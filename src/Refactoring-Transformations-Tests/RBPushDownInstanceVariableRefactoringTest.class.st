Class {
	#name : 'RBPushDownInstanceVariableRefactoringTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'running' }
RBPushDownInstanceVariableRefactoringTest >> setUp [

	super setUp.
	model := self modelOnClasses: { RBBasicLintRuleTestData . RBLintRuleTestData }
]

{ #category : 'failure tests' }
RBPushDownInstanceVariableRefactoringTest >> testShouldWarnWhenVariableReferenced [

	self shouldWarn: (RBPushDownInstanceVariableRefactoring
					 model: model
					 variable: 'name'
					 class: #RBBasicLintRuleTestData)
]
