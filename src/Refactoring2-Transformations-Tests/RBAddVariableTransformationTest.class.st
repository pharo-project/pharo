Class {
	#name : #RBAddVariableTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #running }
RBAddVariableTransformationTest >> setUp [

	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #running }
RBAddVariableTransformationTest >> testAddClassVariable [

	| refactoring |
	refactoring := (RBAddVariableTransformation 
						classVariable: 'Asdf'
						class: #RBTransformationRuleTestData)
						asRefactoring transform.
	
	self assert: ((refactoring model
		classNamed: #RBTransformationRuleTestData)
		directlyDefinesClassVariable: #Asdf)
]

{ #category : #running }
RBAddVariableTransformationTest >> testAddInstanceVariable [

	| refactoring |
	refactoring := (RBAddVariableTransformation 
						instanceVariable: 'asdf'
						class: #RBTransformationRuleTestData)
						asRefactoring transform.
		
	self assert: ((refactoring model
		classNamed: #RBTransformationRuleTestData)
		directlyDefinesInstanceVariable: 'asdf')
]

{ #category : #running }
RBAddVariableTransformationTest >> testAlreadyExistingName [

	self
		shouldFail: (RBAddVariableTransformation 
						instanceVariable: 'class'
						class: #RBTransformationRuleTestData)
						asRefactoring;
		shouldFail: (RBAddVariableTransformation 
						instanceVariable: 'name'
						class: #RBTransformationRuleTestData)
						asRefactoring;
		shouldFail: (RBAddVariableTransformation 
						classVariable: #RecursiveSelfRule
						class: #RBTransformationRuleTestData)
						asRefactoring;
		shouldFail: (RBAddVariableTransformation 
						classVariable: self objectClassVariable
						class: #RBTransformationRuleTestData)
						asRefactoring 
]

{ #category : #running }
RBAddVariableTransformationTest >> testModelAddClassVariable [

	| refactoring |
	refactoring := (RBAddVariableTransformation 
						model: model
						classVariable: #ClassVarName3
						class: (model classNamed: #Bar))
						asRefactoring transform.
	
	self assert: ((model classNamed: #Bar)
		  directlyDefinesClassVariable: #ClassVarName3)
]

{ #category : #running }
RBAddVariableTransformationTest >> testModelAlreadyExistingName [

	self 
		shouldFail: (RBAddVariableTransformation 
						model: model
						instanceVariable: 'instVarName1'
						class: (model classNamed: #Bar))
						asRefactoring ;
		shouldFail: (RBAddVariableTransformation 
						model: model
						classVariable: #ClassVarName2
						class: (model classNamed: #Bar))
						asRefactoring 
]

{ #category : #running }
RBAddVariableTransformationTest >> testModelMetaclassAlreadyExistingName [

	(model metaclassNamed: #Foo)
		addInstanceVariable: 'instVarName1'.
		
	self shouldFail: (RBAddVariableTransformation
							model: model
							instanceVariable: 'instVarName1'
							class: (model classNamed: #Bar))
							asRefactoring 
]

{ #category : #running }
RBAddVariableTransformationTest >> testNewExistingName [

	model := RBNamespace2 new.
	model name: 'Add instance variable'.
	model defineClass: 'Object subclass: #FOOBAR 
				instanceVariableNames: ''asdf'' 	
				classVariableNames: ''''
				poolDictionaries: ''''
				category: #''Refactory-Test data'''.

	self shouldFail: (RBAddVariableTransformation  
							model: model
							instanceVariable: 'asdf'
							class: (model classNamed: #FOOBAR))
							asRefactoring 
]

{ #category : #running }
RBAddVariableTransformationTest >> testNewHierarchyExistingName [

	model := RBNamespace2 new.
	model name: 'Add instance variable'.
	model defineClass: 'Object subclass: #FOOBAR 
				instanceVariableNames: ''asdf'' 	
				classVariableNames: ''''
				poolDictionaries: ''''
				category: #''Refactory-Test data'''.
	model defineClass: 'FOOBAR subclass: #BARFOO 
				instanceVariableNames: '''' 	
				classVariableNames: ''''
				poolDictionaries: ''''
				category: ''Refactory-Test data'''.
				
	self shouldFail: (RBAddVariableTransformation
							model: model
							instanceVariable: 'asdf'
							class: (model classNamed: #BARFOO))
							asRefactoring 
]

{ #category : #running }
RBAddVariableTransformationTest >> testRefactoring [

	| refactoring |
	model := RBNamespace2 new.
	model name: 'Add instance variable'.
	model defineClass: 'Object subclass: #FOOBAR 
				instanceVariableNames: ''fdsa'' 	
				classVariableNames: ''''
				poolDictionaries: ''''
				category: #''Refactory-Test data'''.
				
	refactoring := (RBAddVariableTransformation 
						model: model
						instanceVariable: 'asdf'
						class: (model classNamed: #FOOBAR))
						asRefactoring transform.
	
	self assert: ((refactoring model
		classNamed: #FOOBAR) 
		directlyDefinesInstanceVariable: 'asdf')
]

{ #category : #testing }
RBAddVariableTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBAddVariableTransformation 
							instanceVariable: 'asdf'
							class: self changeMock name)
							transform. 
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self changeMock name asSymbol.
	self assert: (class directlyDefinesInstanceVariable: 'asdf').
]
