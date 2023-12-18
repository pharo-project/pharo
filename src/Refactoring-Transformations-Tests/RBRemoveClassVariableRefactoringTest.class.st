Class {
	#name : 'RBRemoveClassVariableRefactoringTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'failure tests' }
RBRemoveClassVariableRefactoringTest >> testShouldWarnWhenVariableReferenced [

	self shouldWarn: (RBRemoveSharedVariableRefactoring
			 variable: #RecursiveSelfRule
			 class: #RBTransformationRuleTestData)
]
