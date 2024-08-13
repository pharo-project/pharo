Class {
	#name : 'RBPullUpInstanceVariableParametrizedTest',
	#superclass : 'RBWithDifferentConstructorsParametrizedTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'tests' }
RBPullUpInstanceVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBPullUpInstanceVariableRefactoring .
					  #constructor -> #variable:class: };
		yourself
]

{ #category : 'running' }
RBPullUpInstanceVariableParametrizedTest >> setUp [

	super setUp.
	model := self modelOnClasses: { RBLintRuleTestData . RBBasicLintRuleTestData . RBFooLintRuleTestData }
]

{ #category : 'failure tests' }
RBPullUpInstanceVariableParametrizedTest >> testFailurePullUpVariableNotDefined [

	self shouldFail: (self createRefactoringWithModel: model andArguments:
			 { 'notDefinedVariable'. #RBLintRuleTestData })
]

{ #category : 'tests' }
RBPullUpInstanceVariableParametrizedTest >> testPullUpInstVar [
	| refactoring |
	refactoring := self createRefactoringWithModel: model andArguments:
			{'result' . #RBLintRuleTestData}.
	[self executeRefactoring: refactoring]
		on: RBRefactoringWarning
		do: [ :e | e resume ].
	self assert: ((refactoring model classNamed: #RBLintRuleTestData)
		directlyDefinesInstanceVariable: 'result').
	self deny: ((refactoring model classNamed: #RBBasicLintRuleTestData)
		directlyDefinesInstanceVariable: 'result').
	self deny: ((refactoring model classNamed: #RBFooLintRuleTestData)
		directlyDefinesInstanceVariable: 'result')
]
