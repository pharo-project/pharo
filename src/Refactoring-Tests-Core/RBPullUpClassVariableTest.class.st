Class {
	#name : #RBPullUpClassVariableTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
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

{ #category : #tests }
RBPullUpClassVariableTest >> testPullUpClassVariable [
	| refactoring |
	refactoring := RBPullUpClassVariableRefactoring 
		variable: #RecursiveSelfRule
		class: RBLintRuleTestData.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBLintRuleTestData) directlyDefinesClassVariable: #RecursiveSelfRule).
	self deny: ((refactoring model classNamed: #RBTransformationRuleTestData) directlyDefinesClassVariable: #RecursiveSelfRule)
]
