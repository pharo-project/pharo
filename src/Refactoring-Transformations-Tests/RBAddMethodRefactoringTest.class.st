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
RBAddMethodRefactoringTest >> testWarnModelExistingSelector [

	self
		shouldWarn: (RBAddMethodRefactoring
				 model: model
				 sourceCode: 'classVarName1 ^super printString'
				 in: (model classNamed: #Bar)
				 withProtocol: #accessing);
		shouldWarn: (RBAddMethodRefactoring
				 model: model
				 sourceCode: 'printString ^super printString'
				 in: (model classNamed: #Bar)
				 withProtocol: #accessing)
]

{ #category : 'failure tests' }
RBAddMethodRefactoringTest >> testWarnModelInheritedSelector [

	| refactoring |
	refactoring := RBAddMethodRefactoring
		               model: model
		               sourceCode: 'printString ^super printString'
		               in: (model classNamed: #Bar)
		               withProtocol: #accessing .
	self shouldWarn: refactoring
]

{ #category : 'failure tests' }
RBAddMethodRefactoringTest >> testWarnWhenSelectorAlreadyExists [

	self shouldWarn: (RBAddMethodRefactoring
			 sourceCode: 'printString ^super printString'
			 in: RBBasicLintRuleTestData
			 withProtocol: #accessing)
]
