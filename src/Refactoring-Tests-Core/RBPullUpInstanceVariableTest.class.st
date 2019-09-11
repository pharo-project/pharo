Class {
	#name : #RBPullUpInstanceVariableTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #tests }
RBPullUpInstanceVariableTest >> testPullUpInstVar [
	| refactoring |
	self proceedThroughWarning: 
		[ refactoring := RBPullUpInstanceVariableRefactoring 
			variable: 'result'
			class: RBLintRuleTestData.
		self executeRefactoring: refactoring ].
	self assert: ((refactoring model classNamed: #RBLintRuleTestData) directlyDefinesInstanceVariable: 'result').
	self deny: ((refactoring model classNamed: #RBBasicLintRuleTestData) directlyDefinesInstanceVariable: 'result')
]

{ #category : #'failure tests' }
RBPullUpInstanceVariableTest >> testPullUpVariableNotDefined [
	self shouldFail: (RBPullUpInstanceVariableRefactoring 
			variable: 'notDefinedVariable'
			class: RBLintRuleTestData)
]
