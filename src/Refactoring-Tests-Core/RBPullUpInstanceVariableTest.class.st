Class {
	#name : #RBPullUpInstanceVariableTest,
	#superclass : #RBRefactoringTest,
	#category : #Refactoring-Tests-Core
}

{ #category : #tests }
RBPullUpInstanceVariableTest >> testPullUpInstVar [
	| refactoring |
	self proceedThroughWarning: 
		[ refactoring := RBPullUpInstanceVariableRefactoring 
			variable: 'result'
			class: RBLintRuleTest.
		self executeRefactoring: refactoring ].
	self assert: ((refactoring model classNamed: #RBLintRuleTest) directlyDefinesInstanceVariable: 'result').
	self deny: ((refactoring model classNamed: #RBBasicLintRuleTest) directlyDefinesInstanceVariable: 'result')
]

{ #category : #'failure tests' }
RBPullUpInstanceVariableTest >> testPullUpVariableNotDefined [
	self shouldFail: (RBPullUpInstanceVariableRefactoring 
			variable: 'notDefinedVariable'
			class: RBLintRuleTest)
]
