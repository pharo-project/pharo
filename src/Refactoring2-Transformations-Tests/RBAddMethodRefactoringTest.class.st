Class {
	#name : #RBAddMethodRefactoringTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Test'
}

{ #category : #running }
RBAddMethodRefactoringTest >> setUp [

	super setUp.
	model := self rbModelForVariableTest
]

{ #category : #'failure tests' }
RBAddMethodRefactoringTest >> testFailureExistingSelector [

	self shouldFail: (RBAddMethodRefactoring
			 addMethod: 'printString ^super printString'
			 toClass: RBBasicLintRuleTestData
			 inProtocols: #( #accessing ))
]

{ #category : #'failure tests' }
RBAddMethodRefactoringTest >> testFailureModelExistingSelector [

	self
		shouldFail: (RBAddMethodRefactoring
				 model: model
				 addMethod: 'classVarName1 ^super printString'
				 toClass: (model classNamed: #Bar)
				 inProtocols: #( #accessing ));
		shouldFail: (RBAddMethodRefactoring
				 model: model
				 addMethod: 'printString ^super printString'
				 toClass: (model classNamed: #Bar)
				 inProtocols: { #accessing })
]

{ #category : #'failure tests' }
RBAddMethodRefactoringTest >> testFailureModelInheritedSelector [

	| refactoring |
	refactoring := RBAddMethodRefactoring
		               model: model
		               addMethod: 'printString ^super printString'
		               toClass: (model classNamed: #Bar)
		               inProtocols: #( #accessing ).
	self shouldFail: refactoring
]
