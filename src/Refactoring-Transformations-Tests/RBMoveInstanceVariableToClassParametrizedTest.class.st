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
	
	| refact |
	refact := (RBMoveInstanceVariableToClassTransformation model: model variable: #result fromClass: #RBFooLintRuleTestData toClass: #RBBasicLintRuleTestData).
	self shouldFail: refact.
	
	"notice that result does not exist is #RBFooLintRuleTestData"
			
]

{ #category : 'failure tests' }
RBMoveInstanceVariableToClassParametrizedTest >> testFailureVariableDoesNotExistInOldClass [
	
	| refact |
	refact := RBMoveInstanceVariableToClassTransformation 
					model: model 
					variable: #doesNotExist 
					fromClass: #RBFooLintRuleTestData 
					toClass: #RBBasicLintRuleTestData.
	self halt.
	self deny: refact preconditions check
	
		
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
