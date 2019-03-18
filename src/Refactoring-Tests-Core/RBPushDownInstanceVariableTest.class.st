Class {
	#name : #RBPushDownInstanceVariableTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #tests }
RBPushDownInstanceVariableTest >> testModelPushDownToMetaclass [
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model metaclassNamed: #SomeClass) addInstanceVariable: 'foo'.
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model metaclassNamed: #Subclass) compile: 'foo ^foo'
		classified: #(#accessing).
	self executeRefactoring: (RBPushDownInstanceVariableRefactoring 
				model: model
				variable: 'foo'
				class: (model metaclassNamed: #SomeClass)).
	self 
		deny: ((model metaclassNamed: #SomeClass) directlyDefinesVariable: 'foo').
	self 
		assert: ((model metaclassNamed: #Subclass) directlyDefinesVariable: 'foo')
]

{ #category : #tests }
RBPushDownInstanceVariableTest >> testModelPushDownToMultipleSubclasses [
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: ''foo'' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model classNamed: #Subclass) compile: 'foo ^foo' classified: #(#accessing).
	model 
		defineClass: 'SomeClass subclass: #AnotherSubclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model classNamed: #AnotherSubclass) compile: 'bar ^foo'
		classified: #(#accessing).
	self executeRefactoring: (RBPushDownInstanceVariableRefactoring 
				model: model
				variable: 'foo'
				class: (model classNamed: #SomeClass)).
	self deny: ((model classNamed: #SomeClass) directlyDefinesVariable: 'foo').
	self 
		assert: ((model classNamed: #Subclass) directlyDefinesVariable: 'foo').
	self assert: ((model classNamed: #AnotherSubclass) 
				directlyDefinesVariable: 'foo')
]

{ #category : #tests }
RBPushDownInstanceVariableTest >> testModelRemoveUnusedVariable [
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: ''foo'' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	self executeRefactoring: (RBPushDownInstanceVariableRefactoring 
				model: model
				variable: 'foo'
				class: (model classNamed: #SomeClass)).
	self deny: ((model classNamed: #SomeClass) directlyDefinesVariable: 'foo').
	self assert: ((model classNamed: #Subclass) directlyDefinesVariable: 'foo')
]

{ #category : #'failure tests' }
RBPushDownInstanceVariableTest >> testNonExistantName [
	self shouldFail: (RBPushDownInstanceVariableRefactoring 
			variable: 'foo'
			class: RBBasicLintRuleTestData)
]

{ #category : #tests }
RBPushDownInstanceVariableTest >> testPushDownInstanceVariable [
	| refactoring |
	refactoring := RBPushDownInstanceVariableRefactoring 
		variable: 'foo1'
		class: RBLintRuleTestData.
	self executeRefactoring: refactoring.
	(refactoring model classNamed: #RBLintRuleTestData) subclasses do: [ :each | self assert: (each directlyDefinesInstanceVariable: 'foo1') ]
]
