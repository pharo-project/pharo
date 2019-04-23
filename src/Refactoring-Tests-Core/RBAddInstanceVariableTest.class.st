Class {
	#name : #RBAddInstanceVariableTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #running }
RBAddInstanceVariableTest >> setUp [
	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #tests }
RBAddInstanceVariableTest >> testAddInstanceVariable [
	| refactoring |
	refactoring := RBAddInstanceVariableRefactoring 
		variable: 'asdf'
		class: RBTransformationRuleTestData.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBTransformationRuleTestData) directlyDefinesInstanceVariable: 'asdf')
]

{ #category : #tests }
RBAddInstanceVariableTest >> testAddToModel [
	| refactoring |
	model := RBClassModelFactory rbNamespace new.
	model name: 'Add instance variable'.
	model 
		defineClass: 'Object subclass: #FOOBAR 
				instanceVariableNames: ''fdsa'' 	
				classVariableNames: ''''
				poolDictionaries: ''''
				category: #''Refactory-Test data'''.
	refactoring := RBAddInstanceVariableRefactoring 
				model: model
				variable: 'asdf'
				class: (model classNamed: #FOOBAR).
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #FOOBAR) 
				directlyDefinesInstanceVariable: 'asdf')
]

{ #category : #'failure tests' }
RBAddInstanceVariableTest >> testAlreadyExistingName [
	self
		shouldFail: (RBAddInstanceVariableRefactoring 
				variable: 'class'
				class: RBTransformationRuleTestData);
		shouldFail: (RBAddInstanceVariableRefactoring 
				variable: 'name'
				class: RBTransformationRuleTestData)
]

{ #category : #'failure tests' }
RBAddInstanceVariableTest >> testModelAlreadyExistingName [
	| refactoring |
	refactoring := RBAddInstanceVariableRefactoring 
				model: model
				variable: 'instVarName1'
				class: (model classNamed: #Bar).
	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBAddInstanceVariableTest >> testModelMetaclassAlreadyExistingName [
	| refactoring |
	(model metaclassNamed: #Foo) addInstanceVariable: 'instVarName1'.
	refactoring := RBAddInstanceVariableRefactoring 
				model: model
				variable: 'instVarName1'
				class: (model classNamed: #Bar).
	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBAddInstanceVariableTest >> testNewExistingName [
	| refactoring |
	model := RBClassModelFactory rbNamespace new.
	model name: 'Add instance variable'.
	model 
		defineClass: 'Object subclass: #FOOBAR 
				instanceVariableNames: ''asdf'' 	
				classVariableNames: ''''
				poolDictionaries: ''''
				category: #''Refactory-Test data'''.
	refactoring := RBAddInstanceVariableRefactoring 
				model: model
				variable: 'asdf'
				class: (model classNamed: #FOOBAR).
	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBAddInstanceVariableTest >> testNewHierarchyExistingName [
	| refactoring |
	model := RBClassModelFactory rbNamespace new.
	model name: 'Add instance variable'.
	model 
		defineClass: 'Object subclass: #FOOBAR 
				instanceVariableNames: ''asdf'' 	
				classVariableNames: ''''
				poolDictionaries: ''''
				category: #''Refactory-Test data'''.
	model 
		defineClass: 'FOOBAR subclass: #BARFOO 
				instanceVariableNames: '''' 	
				classVariableNames: ''''
				poolDictionaries: ''''
				category: ''Refactory-Test data'''.
	refactoring := RBAddInstanceVariableRefactoring 
				model: model
				variable: 'asdf'
				class: (model classNamed: #BARFOO).
	self shouldFail: refactoring
]
