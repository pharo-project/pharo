Class {
	#name : #RBMoveInstanceVariableToClassParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-SingleParametrized'
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
	refactoring := self
		               createRefactoringWithModel: model
		               andArguments:
		               { 'methodBlock'. #RBBasicLintRuleTestData.
		               #RBFooLintRuleTestData }.
	oldClass := refactoring model classNamed: #RBBasicLintRuleTestData.
	newClass := refactoring model classNamed: #RBFooLintRuleTestData.
	self assert:
		(oldClass directlyDefinesInstanceVariable: 'methodBlock').
	self deny: (newClass directlyDefinesInstanceVariable: 'methodBlock').
	self shouldFail: refactoring.
	
	"This test is now expected to fail since transform raises non-resumable RBRefactoringError.
	The test expected resumable RBRefactoringError to be raised and we changed this in #13367.
	Previously it would catch RBRefactoringError and resume."
	"[ refactoring transform ]
		on: RBRefactoringWarning
		do: [ :e | e resume ].
	self assert: refactoring model changes changes size equals: 2.
	oldClass := refactoring model classNamed: #RBBasicLintRuleTestData.
	newClass := refactoring model classNamed: #RBFooLintRuleTestData.
	self deny: (oldClass directlyDefinesInstanceVariable: 'methodBlock').
	self assert:
		(newClass directlyDefinesInstanceVariable: 'methodBlock')"
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
