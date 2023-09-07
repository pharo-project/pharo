Class {
	#name : #RBRemoveMethodsRefactoringTest,
	#superclass : #RBAbstractTransformationTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBRemoveMethodsRefactoringTest >> testFailureNonExistantSelector [

	self shouldFail: (RBRemoveMethodsRefactoring
								selectors: #(#nonExistingMethod)
								from: RBLintRuleTestData )
]
