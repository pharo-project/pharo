Class {
	#name : #RBPullUpClassVariableTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core-Refactorings'
}

{ #category : #'failure tests' }
RBPullUpClassVariableTest >> testMetaClassFailure [
	self shouldFail: (RBPullUpClassVariableRefactoring 
			variable: #RecursiveSelfRule
			class: RBLintRuleTestData class)
]

{ #category : #'failure tests' }
RBPullUpClassVariableTest >> testNonExistantName [
	self shouldFail: (RBPullUpClassVariableRefactoring 
			variable: #Foo
			class: RBLintRuleTestData)
]
