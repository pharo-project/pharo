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
				  (#constructor -> #selector:from:).
				  (#isComposed -> false) };
		  addCase: {
				  (#rbClass -> RBRemoveMethodRefactoring).
				  (#constructor -> #removeMethods:from:).
				  (#isComposed -> true) };
		  
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
			 andArgumentsColl: #( #nonExistingMethod )
			 andOtherArgs: { #RBBasicLintRuleTestData })
]

{ #category : #tests }
RBRemoveMethodParametrizedTest >> testRemoveMethod [
	| refactoring selectors |
	selectors := Array with: 'selectorNotReferenced' asSymbol.
	refactoring := self createRefactoringWithModel: model
		andArgumentsColl: selectors
		andOtherArgs: { #RBClassDataForRefactoringTest }.

	selectors do: [ :symbol |
		self assert:
			(( model classNamed: #RBClassDataForRefactoringTest)
				directlyDefinesMethod: symbol).
	].

	self executeRefactorings: refactoring.

	selectors do: [ :symbol |
		self deny:
			(( model classNamed: #RBClassDataForRefactoringTest)
				directlyDefinesMethod: symbol)
	]
]
