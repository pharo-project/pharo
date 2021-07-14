Class {
	#name : #RBMoveInstanceVariableToClassParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBMoveInstanceVariableToClassParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBMoveInstVarToClassRefactoring };
		addCase: { #rbClass -> RBMoveInstanceVariableToClassTransformation };
		yourself
]

{ #category : #tests }
RBMoveInstanceVariableToClassParametrizedTest >> testRefactoring [

	| refactoring oldClass newClass |
	refactoring := (rbClass
						model: model
						variable: 'methodBlock'
						fromClass: #RBBasicLintRuleTestData
						toClass: #RBFooLintRuleTestData)
						asRefactoring.
				
	oldClass := refactoring model classNamed: #RBBasicLintRuleTestData.
	newClass := refactoring model classNamed: #RBFooLintRuleTestData.
	self assert: (oldClass directlyDefinesInstanceVariable: 'methodBlock').
	self deny: (newClass directlyDefinesInstanceVariable: 'methodBlock').
	
	[ refactoring transform ] on: RBRefactoringError do: [ :e | e resume ].
	self assert: refactoring model changes changes size equals: 2.

	oldClass := refactoring model classNamed: #RBBasicLintRuleTestData.
	newClass := refactoring model classNamed: #RBFooLintRuleTestData.
	self deny: (oldClass directlyDefinesInstanceVariable: 'methodBlock').
	self assert: (newClass directlyDefinesInstanceVariable: 'methodBlock').
	
]

{ #category : #'failure tests' }
RBMoveInstanceVariableToClassParametrizedTest >> testVariableAlreadyExists [

	self shouldFail: (rbClass
		model: model
		variable: 'result'
		fromClass: #RBFooLintRuleTestData
		toClass: #RBBasicLintRuleTestData)
		asRefactoring
]

{ #category : #'failure tests' }
RBMoveInstanceVariableToClassParametrizedTest >> testVariableDoesNotExistInOldClass [

	self shouldFail: (rbClass
		model: model
		variable: 'abc'
		fromClass: #RBFooLintRuleTestData
		toClass: #RBBasicLintRuleTestData)
		asRefactoring
]
