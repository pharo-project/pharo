Class {
	#name : #RBPushDownInstanceVariableParametrizedTest,
	#superclass : #RBParametrizedTestWithDifferentConstructors,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBPushDownInstanceVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBPushDownInstanceVariableRefactoring .
					  #constructor -> #variable:class: };
		addCase: { #rbClass -> RBPushDownVariableTransformation .
					  #constructor -> #instanceVariable:class: };
		yourself
]

{ #category : #tests }
RBPushDownInstanceVariableParametrizedTest >> testModelPushDownToMetaclass [
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model metaclassNamed: #SomeClass) addInstanceVariable: 'foo'.
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model metaclassNamed: #Subclass) compile: 'foo ^foo'
		classified: #(#accessing).
	self executeRefactoring: (self createRefactoringWithModel: model 
		andArguments: {'foo' . #'SomeClass class'}).
	self 
		deny: ((model metaclassNamed: #SomeClass) directlyDefinesVariable: 'foo').
	self 
		assert: ((model metaclassNamed: #Subclass) directlyDefinesVariable: 'foo')
]

{ #category : #tests }
RBPushDownInstanceVariableParametrizedTest >> testModelPushDownToMultipleSubclasses [
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: ''foo'' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model classNamed: #Subclass) compile: 'foo ^foo' classified: #(#accessing).
	model 
		defineClass: 'SomeClass subclass: #AnotherSubclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model classNamed: #AnotherSubclass) compile: 'bar ^foo'
		classified: #(#accessing).
	self executeRefactoring: (self createRefactoringWithModel: model 
		andArguments: {'foo' . #SomeClass}).
	self deny: ((model classNamed: #SomeClass) directlyDefinesVariable: 'foo').
	self 
		assert: ((model classNamed: #Subclass) directlyDefinesVariable: 'foo').
	self assert: ((model classNamed: #AnotherSubclass) 
				directlyDefinesVariable: 'foo')
]

{ #category : #tests }
RBPushDownInstanceVariableParametrizedTest >> testModelRemoveUnusedVariable [
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: ''foo'' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	self executeRefactoring: (self createRefactoringWithModel: model 
		andArguments: {'foo' . #SomeClass}).
	self deny: ((model classNamed: #SomeClass) directlyDefinesVariable: 'foo').
	self assert: ((model classNamed: #Subclass) directlyDefinesVariable: 'foo')
]

{ #category : #'failure tests' }
RBPushDownInstanceVariableParametrizedTest >> testNonExistantName [
	self shouldFail: 
		(self createRefactoringWithArguments: {'foo' . #RBBasicLintRuleTestData})
]

{ #category : #tests }
RBPushDownInstanceVariableParametrizedTest >> testPushDownInstanceVariable [
	| refactoring |
	refactoring := self createRefactoringWithArguments: 
		{'foo1' . #RBLintRuleTestData}.
	self executeRefactoring: refactoring.
	(refactoring model classNamed: #RBLintRuleTestData) subclasses do: [ :each | self assert: (each directlyDefinesInstanceVariable: 'foo1') ]
]
