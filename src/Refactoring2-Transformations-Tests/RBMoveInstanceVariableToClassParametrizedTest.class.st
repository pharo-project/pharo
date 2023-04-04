Class {
	#name : #RBMoveInstanceVariableToClassParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #'building suites' }
RBMoveInstanceVariableToClassParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBMoveInstanceVariableToClassTransformation };
		yourself
]

{ #category : #accessing }
RBMoveInstanceVariableToClassParametrizedTest >> constructor [
	^ #variable:fromClass:toClass:
]

{ #category : #tests }
RBMoveInstanceVariableToClassParametrizedTest >> testRefactoring [

	| refactoring oldClass newClass |
	refactoring := (self createRefactoringWithModel: model
		andArguments: { 'methodBlock' . #RBBasicLintRuleTestData . #RBFooLintRuleTestData }).

	oldClass := refactoring model classNamed: #RBBasicLintRuleTestData.
	newClass := refactoring model classNamed: #RBFooLintRuleTestData.
	self assert: (oldClass directlyDefinesInstanceVariable: 'methodBlock').
	self deny: (newClass directlyDefinesInstanceVariable: 'methodBlock').

	self flag: #shouldRemoveRefactoringError.
	[ refactoring transform ] on: RBRefactoringWarning, RBRefactoringError do: [ :e | e resume ].
	self assert: refactoring model changes changes size equals: 2.

	oldClass := refactoring model classNamed: #RBBasicLintRuleTestData.
	newClass := refactoring model classNamed: #RBFooLintRuleTestData.
	self deny: (oldClass directlyDefinesInstanceVariable: 'methodBlock').
	self assert: (newClass directlyDefinesInstanceVariable: 'methodBlock')
]

{ #category : #'failure tests' }
RBMoveInstanceVariableToClassParametrizedTest >> testVariableAlreadyExists [

	self shouldFail:
		(self createRefactoringWithModel: model
			andArguments: { 'result' . #RBFooLintRuleTestData . #RBBasicLintRuleTestData })
]

{ #category : #'failure tests' }
RBMoveInstanceVariableToClassParametrizedTest >> testVariableDoesNotExistInOldClass [

	self shouldFail:
		(self createRefactoringWithModel: model
			andArguments: { 'abc' . #RBFooLintRuleTestData . #RBBasicLintRuleTestData })
]
