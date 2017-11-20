Class {
	#name : #RBPullUpClassVariableTest,
	#superclass : #RBRefactoringTest,
	#category : #Refactoring-Tests-Core
}

{ #category : #'failure tests' }
RBPullUpClassVariableTest >> testMetaClassFailure [
	self shouldFail: (RBPullUpClassVariableRefactoring 
			variable: #RecursiveSelfRule
			class: RBLintRuleTest class)
]

{ #category : #'failure tests' }
RBPullUpClassVariableTest >> testNonExistantName [
	self shouldFail: (RBPullUpClassVariableRefactoring 
			variable: #Foo
			class: RBLintRuleTest)
]

{ #category : #tests }
RBPullUpClassVariableTest >> testPullUpClassVariable [
	| refactoring |
	refactoring := RBPullUpClassVariableRefactoring 
		variable: #RecursiveSelfRule
		class: RBLintRuleTest.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBLintRuleTest) directlyDefinesClassVariable: #RecursiveSelfRule).
	self deny: ((refactoring model classNamed: #RBTransformationRuleTest) directlyDefinesClassVariable: #RecursiveSelfRule)
]
