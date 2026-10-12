Class {
	#name : 'RBRemoveInstanceVariableRefactoringTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'running' }
RBRemoveInstanceVariableRefactoringTest >> setUp [

	super setUp.
	model := self modelOnClasses: { RBLintRuleTestData }
]

{ #category : 'failure tests' }
RBRemoveInstanceVariableRefactoringTest >> testShouldWarnWhenVariableReferenced [

	self shouldWarn: (ReRemoveInstanceVariableRefactoring
					 model: model
					 variable: 'name'
					 class: #RBLintRuleTestData)
]
