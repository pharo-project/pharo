Class {
	#name : #RBRemoveClassVariableRefactoringTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Test'
}

{ #category : #'failure tests' }
RBRemoveClassVariableRefactoringTest >> testFailureReferencedVariable [

	self shouldWarn: (RBRemoveClassVariableRefactoring
			 variable: #RecursiveSelfRule
			 class: #RBTransformationRuleTestData)
]
