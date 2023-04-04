Class {
	#name : #RBPullUpInstanceVariableParametrizedTest,
	#superclass : #RBWithDifferentConstructorsParametrizedTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBPullUpInstanceVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBPullUpInstanceVariableRefactoring .
					  #constructor -> #variable:class: };
		addCase: { #rbClass -> RBPullUpVariableTransformation .
					  #constructor -> #instanceVariable:class: };
		yourself
]

{ #category : #tests }
RBPullUpInstanceVariableParametrizedTest >> testPullUpInstVar [
	| refactoring |
	refactoring := self createRefactoringWithArguments:
			{'result' . #RBLintRuleTestData}.
	self flag: #shouldRemoveRefactoringError.
	[self executeRefactoring: refactoring]
		on: RBRefactoringError, RBRefactoringWarning
		do: [ :e | e resume ].
	self assert: ((refactoring model classNamed: #RBLintRuleTestData)
		directlyDefinesInstanceVariable: 'result').
	self deny: ((refactoring model classNamed: #RBBasicLintRuleTestData)
		directlyDefinesInstanceVariable: 'result').
	self deny: ((refactoring model classNamed: #RBFooLintRuleTestData)
		directlyDefinesInstanceVariable: 'result')
]

{ #category : #'failure tests' }
RBPullUpInstanceVariableParametrizedTest >> testPullUpVariableNotDefined [
	self shouldFail: (self createRefactoringWithArguments:
		{ 'notDefinedVariable' . #RBLintRuleTestData })
]
