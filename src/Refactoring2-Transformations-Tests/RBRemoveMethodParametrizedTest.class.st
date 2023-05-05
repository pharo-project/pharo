Class {
	#name : #RBRemoveMethodParametrizedTest,
	#superclass : #RBComposeParametrizedTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBRemoveMethodParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBRemoveMethodRefactoring .
					  #constructor -> #removeMethods:from: .
					  #isComposed -> true};
		addCase: { #rbClass -> RBRemoveMethodTransformation .
					  #constructor -> #selector:from: .
					  #isComposed -> false};
		yourself
]

{ #category : #builder }
RBRemoveMethodParametrizedTest >> createRefactoringWithModel: rbNamespace andArguments: aParameterCollection [
	^ (rbClass
		perform: #model: , constructor
		withArguments: {rbNamespace}, aParameterCollection) asRefactoring
]

{ #category : #builder }
RBRemoveMethodParametrizedTest >> createRefactoringWithModel: rbNamespace andArgumentsColl: aParameterCollection andOtherArgs: argCollection [
	^ isComposed
	ifTrue: [ {self createRefactoringWithModel: rbNamespace
		andArguments: {aParameterCollection}, argCollection} ]
	ifFalse: [ aParameterCollection collect: [ :e |
		self createRefactoringWithModel: rbNamespace
		andArguments: {e}, argCollection
	] ]
]

{ #category : #tests }
RBRemoveMethodParametrizedTest >> testModelRecursiveMethodThatIsNotReferencedFromOtherMethods [
	| class otherClass |
	class := model classNamed: #Object.
	class compile: 'someMethodThatIsNotReferenced ^2' classified: #(#accessing).
	self assert: (class definesMethod: 'someMethodThatIsNotReferenced' asSymbol).
	otherClass := model metaclassNamed: self class superclass name.
	otherClass
		compile: 'someMethodThatIsNotReferenced ^3 someMethodThatIsNotReferenced'
		classified: #(#accessing).
	model removeClassNamed: self class name.

	self executeRefactorings: (self createRefactoringWithModel: model
		andArgumentsColl: {'someMethodThatIsNotReferenced' asSymbol}
		andOtherArgs: { #Object }).
	self deny: (class definesMethod: 'someMethodThatIsNotReferenced' asSymbol)
]

{ #category : #tests }
RBRemoveMethodParametrizedTest >> testRemoveMethod [
	| refactoring selectors |
	selectors := Array with: 'selectorNotReferenced' asSymbol.
	refactoring := self createRefactoringWithModel: model
		andArgumentsColl: selectors
		andOtherArgs: { #RBRefactoryTestDataApp }.

	selectors do: [ :symbol |
		self assert:
			(( model classNamed: #RBRefactoryTestDataApp)
				directlyDefinesMethod: symbol).
	].

	self executeRefactorings: refactoring.

	selectors do: [ :symbol |
		self deny:
			(( model classNamed: #RBRefactoryTestDataApp)
				directlyDefinesMethod: symbol)
	]
]

{ #category : #'failure tests' }
RBRemoveMethodParametrizedTest >> testRemoveNonExistingMethod [
	self shouldFail: (self createRefactoringWithModel: model
	andArgumentsColl: #(#nonExistingMethod )
	andOtherArgs: {#RBBasicLintRuleTestData})
]

{ #category : #'failure tests' }
RBRemoveMethodParametrizedTest >> testRemoveReferenced [
	self shouldWarn: (self createRefactoringWithModel: model
	andArgumentsColl:  #(#checkClass:)
	andOtherArgs: { #RBBasicLintRuleTestData })
]

{ #category : #'failure tests' }
RBRemoveMethodParametrizedTest >> testRemoveSameMethodButSendsSuper [
	<expectedFailure>
	self shouldWarn: (self createRefactoringWithModel: model
			andArgumentsColl: #(#new )
			andOtherArgs: {#'RBBasicLintRuleTestData class'})
]
