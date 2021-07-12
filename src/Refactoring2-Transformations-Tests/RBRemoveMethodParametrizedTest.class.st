Class {
	#name : #RBRemoveMethodParametrizedTest,
	#superclass : #RBComposeParametrizedTest,
	#instVars : [
		'index'
	],
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
	class compile: 'someMethodThatIsNotReferenced1 ^2' classified: #(#accessing).
	self assert: (class definesMethod: #someMethodThatIsNotReferenced1).
	otherClass := model metaclassNamed: self class superclass name.
	otherClass 
		compile: 'someMethodThatIsNotReferenced1 ^3 someMethodThatIsNotReferenced1'
		classified: #(#accessing).
	model removeClassNamed: self class name.
	self executeRefactorings: (self createRefactoringWithModel: model
		andArgumentsColl: #(#someMethodThatIsNotReferenced1) 
		andOtherArgs: { #Object }).
	self deny: (class definesMethod: #someMethodThatIsNotReferenced1)
]

{ #category : #tests }
RBRemoveMethodParametrizedTest >> testRemoveMethod [
	| refactoring selectors |
	selectors := Array with: ('selectorNot' , 'Referenced') asSymbol.
	refactoring := self createRefactoringWithModel: model
		andArgumentsColl: selectors 
		andOtherArgs: { #RBRefactoryTestDataApp }.
	self assert: (( model classNamed: #RBRefactoryTestDataApp) directlyDefinesMethod: selectors first).
	self executeRefactorings: refactoring.
	self deny: (( model classNamed: #RBRefactoryTestDataApp) directlyDefinesMethod: selectors first)
]

{ #category : #'failure tests' }
RBRemoveMethodParametrizedTest >> testRemoveNonExistingMethod [
	self shouldFail: (self createRefactoringWithModel: model
	andArgumentsColl: #(#nonExistingMethod )
	andOtherArgs: {#RBBasicLintRuleTestData})
]

{ #category : #'failure tests' }
RBRemoveMethodParametrizedTest >> testRemoveReferenced [
	self shouldFail: (self createRefactoringWithModel: model 
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
