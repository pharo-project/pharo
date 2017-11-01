Class {
	#name : #RBRemoveClassVariableTest,
	#superclass : #RBRefactoringTest,
	#category : 'Refactoring-Tests-Core'
}

{ #category : #'failure tests' }
RBRemoveClassVariableTest >> testNonExistantName [
	self shouldFail: (RBRemoveClassVariableRefactoring 
			variable: #RecursiveSelfRule1
			class: RBTransformationRuleTest)
]

{ #category : #'failure tests' }
RBRemoveClassVariableTest >> testReferencedVariable [
	self shouldFail: (RBRemoveClassVariableRefactoring 
			variable: #RecursiveSelfRule
			class: RBTransformationRuleTest)
]

{ #category : #tests }
RBRemoveClassVariableTest >> testRemoveClassVar [
	| refactoring class |
	refactoring := RBRemoveClassVariableRefactoring 
		variable: 'Foo1'
		class: RBLintRuleTest.
	class := refactoring model classNamed: #RBLintRuleTest.
	self assert: (class definesClassVariable: 'Foo1').
	self executeRefactoring: refactoring.
	self deny: (class definesClassVariable: 'Foo1')
]
