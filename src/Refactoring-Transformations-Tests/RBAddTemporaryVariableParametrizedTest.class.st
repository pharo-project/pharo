Class {
	#name : 'RBAddTemporaryVariableParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'tests' }
RBAddTemporaryVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBAddTemporaryVariableTransformation };
		yourself
]

{ #category : 'accessing' }
RBAddTemporaryVariableParametrizedTest >> constructor [
	^ #variable:inMethod:inClass:
]

{ #category : 'tests' }
RBAddTemporaryVariableParametrizedTest >> testAddTemporaryRefactoring [

	| refactoring class |
	refactoring := (self createRefactoringWithArguments:
		{ 'variable2' . #methodBefore . #RBAddReturnStatementTransformationTest }).
	self executeRefactoring: refactoring.

	self assert: refactoring model changes changes size equals: 1.

	class := refactoring model classNamed: #RBAddReturnStatementTransformationTest.
	self assert: (class directlyDefinesMethod: #methodBefore).
	self assert: (class parseTreeForSelector: #methodBefore) temporaries size equals: 2
]

{ #category : 'failure tests' }
RBAddTemporaryVariableParametrizedTest >> testFailureClassDoesNotExist [

	self shouldFail: (self createRefactoringWithArguments:
			 { 'variable2'. #methodBefore. #RBReturnStatementTransformationTest })
]

{ #category : 'failure tests' }
RBAddTemporaryVariableParametrizedTest >> testFailureMethodDoesNotExist [

	self shouldFail: (self createRefactoringWithArguments:
			 { 'variable2'. #method. #RBAddReturnStatementTransformationTest })
]
