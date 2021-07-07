Class {
	#name : #RBAddInstanceVariableParametrizedTest,
	#superclass : #RBVariableParametrizedTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBAddInstanceVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBAddInstanceVariableRefactoring .
					  #constructor -> #variable:class: };
		addCase: { #rbClass -> RBAddVariableTransformation .
					  #constructor -> #instanceVariable:class: };
		yourself
]

{ #category : #running }
RBAddInstanceVariableParametrizedTest >> setUp [
	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #tests }
RBAddInstanceVariableParametrizedTest >> testAddInstanceVariable [
	| refactoring |
	refactoring := self createRefactoringWithArguments: 
		{'asdf' . #RBTransformationRuleTestData }.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBTransformationRuleTestData) directlyDefinesInstanceVariable: 'asdf')
]

{ #category : #tests }
RBAddInstanceVariableParametrizedTest >> testAddToModel [
	| refactoring |
	model := RBNamespace2 new.
	model name: 'Add instance variable'.
	model 
		defineClass: 'Object subclass: #FOOBAR 
				instanceVariableNames: ''fdsa'' 	
				classVariableNames: ''''
				poolDictionaries: ''''
				category: #''Refactory-Test data'''.
	refactoring := self createRefactoringWithModel: model 
		andArguments: {'asdf' .  #FOOBAR }.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #FOOBAR) 
				directlyDefinesInstanceVariable: 'asdf')
]

{ #category : #'failure tests' }
RBAddInstanceVariableParametrizedTest >> testAlreadyExistingName [
	self
		shouldFail: (self createRefactoringWithArguments: { 'class' . #RBTransformationRuleTestData});
		shouldFail: (self createRefactoringWithArguments: {'name'. #RBTransformationRuleTestData})
]

{ #category : #'failure tests' }
RBAddInstanceVariableParametrizedTest >> testModelAlreadyExistingName [
	| refactoring |
	refactoring := self createRefactoringWithModel: model
		andArguments: {'instVarName1' . #Bar }.
	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBAddInstanceVariableParametrizedTest >> testModelMetaclassAlreadyExistingName [
	| refactoring |
	(model metaclassNamed: #Foo) addInstanceVariable: 'instVarName1'.
	refactoring := self createRefactoringWithModel: model 
		andArguments: {'instVarName1' . #Bar }.
	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBAddInstanceVariableParametrizedTest >> testNewExistingName [
	| refactoring |
	model := RBClassModelFactory rbNamespace new.
	model name: 'Add instance variable'.
	model 
		defineClass: 'Object subclass: #FOOBAR 
				instanceVariableNames: ''asdf'' 	
				classVariableNames: ''''
				poolDictionaries: ''''
				category: #''Refactory-Test data'''.
	refactoring := self createRefactoringWithModel: model 
	andArguments: {'asdf' . #FOOBAR }.
	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBAddInstanceVariableParametrizedTest >> testNewHierarchyExistingName [
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
	refactoring := self createRefactoringWithModel: model 
		andArguments: { 'asdf' .#BARFOO }.
	self shouldFail: refactoring
]
