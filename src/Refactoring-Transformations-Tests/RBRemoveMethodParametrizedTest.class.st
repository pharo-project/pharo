Class {
	#name : 'RBRemoveMethodParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
RBRemoveMethodParametrizedTest class >> testParameters [

	^ ParametrizedTestMatrix new
		  addCase: { (#rbClass -> RBRemoveMethodRefactoring) };
		  addCase: { (#rbClass -> RBRemoveMethodTransformation) };
		  yourself
]

{ #category : 'builder' }
RBRemoveMethodParametrizedTest >> createRefactoringWithModel: rbNamespace andArguments: aParameterCollection [

	^ rbClass
		  perform: #model: , #selector:from:
		  withArguments: { rbNamespace } , aParameterCollection
]

{ #category : 'failure tests' }
RBRemoveMethodParametrizedTest >> testFailureRemoveNonExistingMethod [

	self shouldFail: (self
			 createRefactoringWithModel: model
			 andArguments: #( #nonExistingMethod #RBBasicLintRuleTestData ))
]

{ #category : 'tests' }
RBRemoveMethodParametrizedTest >> testRemoveMethod [
	| refactoring selector |
	selector := 'selectorNotReferenced' asSymbol.
	refactoring := self createRefactoringWithModel: model
		andArguments: { selector . #RBClassDataForRefactoringTest }.

	self assert:
		(( model classNamed: #RBClassDataForRefactoringTest)
			directlyDefinesMethod: selector).

	self executeRefactoring: refactoring.

	self deny:
		(( model classNamed: #RBClassDataForRefactoringTest)
			directlyDefinesMethod: selector)
]
