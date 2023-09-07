Class {
	#name : #RBRemoveMethodParametrizedTest,
	#superclass : #RBWithDifferentConstructorsParametrizedTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBRemoveMethodParametrizedTest class >> testParameters [

	^ ParametrizedTestMatrix new
		  addCase: {
				  (#rbClass -> RBRemoveMethodTransformation).
				  (#constructor -> #selector:from:) };
		  addCase: {
				  (#rbClass -> RBRemoveMethodRefactoring).
				  (#constructor -> #selector:from:) } 
		  yourself
]

{ #category : #builder }
RBRemoveMethodParametrizedTest >> createRefactoringWithModel: rbNamespace andArguments: aParameterCollection [

	^ rbClass
		  perform: #model: , constructor
		  withArguments: { rbNamespace } , aParameterCollection
]

{ #category : #'failure tests' }
RBRemoveMethodParametrizedTest >> testFailureRemoveNonExistingMethod [

	self shouldFail: (self
			 createRefactoringWithModel: model
			 andArguments: #( #nonExistingMethod #RBBasicLintRuleTestData ))
]

{ #category : #tests }
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
