Class {
	#name : #RBRemoveClassVariableRefactoringTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Test'
}

{ #category : #'failure tests' }
RBRemoveClassVariableRefactoringTest >> testShouldWarnWhenVariableReferenced [

	self shouldWarn: (RBRemoveClassVariableRefactoring
			 variable: #RecursiveSelfRule
			 class: #RBTransformationRuleTestData)
]
