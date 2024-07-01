Class {
	#name : 'RBMoveInstanceVariableToClassParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'building suites' }
RBMoveInstanceVariableToClassParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBMoveInstanceVariableToClassTransformation };
		yourself
]

{ #category : 'accessing' }
RBMoveInstanceVariableToClassParametrizedTest >> constructor [
	^ #variable:fromClass:toClass:
]

{ #category : 'failure tests' }
RBMoveInstanceVariableToClassParametrizedTest >> testFailureVariableAlreadyExists [

	
	self assert: (RBBasicLintRuleTestData instVarNames includes: #result).
	self should: [ (RBMoveInstanceVariableToClassTransformation 
		model: model 
		variable: #result 
		fromClass: #RBBasicLintRuleTestData 
		toClass: #RBBasicLintRuleTestData) generateChanges ] raise: RBRefactoringError
]

{ #category : 'failure tests' }
RBMoveInstanceVariableToClassParametrizedTest >> testFailureVariableDoesNotExistInOldClass [

	self deny: (RBFooLintRuleTestData instVarNames includes: #abc).
	self should: [ (RBMoveInstanceVariableToClassTransformation 
		model: model 
		variable: #abc 
		fromClass: #RBFooLintRuleTestData 
		toClass: #RBBasicLintRuleTestData) generateChanges ] raise: RBRefactoringError
	
]

{ #category : 'tests' }
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

	refactoring generateChanges.
	self assert: refactoring model changes changes size equals: 2.
	oldClass := refactoring model classNamed: #RBBasicLintRuleTestData.
	newClass := refactoring model classNamed: #RBFooLintRuleTestData.
	self deny: (oldClass directlyDefinesInstanceVariable: 'methodBlock').
	self assert:
		(newClass directlyDefinesInstanceVariable: 'methodBlock')
]
