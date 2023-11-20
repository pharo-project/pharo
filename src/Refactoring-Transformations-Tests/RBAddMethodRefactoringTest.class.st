Class {
	#name : 'RBAddMethodRefactoringTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'running' }
RBAddMethodRefactoringTest >> setUp [

	super setUp.
	model := self rbModelForVariableTest
]

{ #category : 'failure tests' }
RBAddMethodRefactoringTest >> testFailureExistingSelector [

	self shouldFail: (RBAddMethodRefactoring
			 sourceCode: 'printString ^super printString'
			 in: RBBasicLintRuleTestData
			 withProtocol: #accessing)
]

{ #category : 'failure tests' }
RBAddMethodRefactoringTest >> testFailureModelExistingSelector [

	self
		shouldFail: (RBAddMethodRefactoring
				 model: model
				 sourceCode: 'classVarName1 ^super printString'
				 in: (model classNamed: #Bar)
				 withProtocol: #accessing);
		shouldFail: (RBAddMethodRefactoring
				 model: model
				 sourceCode: 'printString ^super printString'
				 in: (model classNamed: #Bar)
				 withProtocol: #accessing)
]

{ #category : 'failure tests' }
RBAddMethodRefactoringTest >> testFailureModelInheritedSelector [

	| refactoring |
	refactoring := RBAddMethodRefactoring
		               model: model
		               sourceCode: 'printString ^super printString'
		               in: (model classNamed: #Bar)
		               withProtocol: #accessing .
	self shouldFail: refactoring
]
