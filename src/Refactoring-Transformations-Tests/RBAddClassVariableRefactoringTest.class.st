Class {
	#name : 'RBAddClassVariableRefactoringTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'running' }
RBAddClassVariableRefactoringTest >> setUp [

	super setUp.
	model := self rbModelForVariableTest
]

{ #category : 'tests' }
RBAddClassVariableRefactoringTest >> testFailureAlreadyExistingName [

	self shouldFail: (RBAddClassVariableRefactoring
			 variable: #RecursiveSelfRule
			 class: #RBTransformationRuleTestData).
	self shouldFail: (RBAddClassVariableRefactoring
				 variable: #objectClassVariable
				 class: #RBTransformationRuleTestData )
]

{ #category : 'tests' }
RBAddClassVariableRefactoringTest >> testFailureModelAlreadyExistingName [

	self shouldFail: (RBAddClassVariableRefactoring
			 model: model
			 variable: #ClassVarName2
			 class: #Bar)
]
