Class {
	#name : #ClyClassScopeTest,
	#superclass : #ClyLocalClassScopeTestCase,
	#category : #'Calypso-SystemQueries-Tests-Scopes'
}

{ #category : #running }
ClyClassScopeTest >> scopeClass [	
	^ClyClassScope
]

{ #category : #tests }
ClyClassScopeTest >> testClassEnumeration [
	scope := ClyClassScope ofAll: {ClyClass1FromP1. ClyClass2FromP1}.
	
	scope classesDo: [ :each | result add: each ].
	
	self assert: result asArray equals: {ClyClass1FromP1. ClyClass2FromP1}
]

{ #category : #tests }
ClyClassScopeTest >> testClassVarsEnumeration [
	scope := ClyClassScope ofAll: {Object. ClyClass1FromP1}.	"last one has no class vars"
		
	scope classVariablesDo: [ :each | result add: each ].
	
	self assert: result size equals: 1.
	self
		assert: result anyOne
		equals: (ClyClassVariable on: Object classVariables first definedIn: Object)
]

{ #category : #tests }
ClyClassScopeTest >> testClassVarsEnumerationFromClassSideOfDefiningClass [
	scope := ClyClassScope of: Object class.
	
	scope classVariablesDo: [ :each | result add: each ].
	
	self assert: result size equals: 1.
	self
		assert: result anyOne
		equals: (ClyClassVariable on: Object classVariables first definedIn: Object)
]

{ #category : #tests }
ClyClassScopeTest >> testClassVarsEnumerationWhenTheyNotExist [
	scope := ClyClassScope of: ClyClass1FromP1.
	
	scope classVariablesDo: [ :each | result add: each ].
	
	self assert: result isEmpty
]

{ #category : #tests }
ClyClassScopeTest >> testInstVarsEnumeration [
	scope := ClyClassScope of: ClyClass1FromP1.

	scope instanceVariablesDo: [ :each | result add: each ].

	self assert: (result as: Set) equals: {
		ClyInstanceVariable named: #instanceSideVar2 definedIn: ClyClass1FromP1.
		ClyInstanceVariable named: #instanceSideVar1 definedIn: ClyClass1FromP1
	} asSet
]

{ #category : #tests }
ClyClassScopeTest >> testInstVarsEnumerationWhenThereAreNoVariables [
	scope := ClyClassScope of: Object.
	
	scope instanceVariablesDo: [ :each | result add: each ].
	
	self assert: result isEmpty
]

{ #category : #tests }
ClyClassScopeTest >> testMethodGroupsEnumeration [
	environment addPlugin: ClyDefaultSystemEnvironmentPlugin new.
	scope := ClyClassScope of: ClyClass1FromP1 in: environment.
	
	scope methodGroupsDo: [ :each | result add: each ].
	
	self
		assert: (result collect: #class as: Set)
		equals: {ClyAllMethodGroup. ClyTaggedMethodGroup} asSet.
	self
		assert: (result
				select: [ :each | each class = ClyTaggedMethodGroup ]
				thenCollect: #tag) sorted asArray
		equals: ClyClass1FromP1 tagsForMethods sorted asArray
]

{ #category : #tests }
ClyClassScopeTest >> testMethodsEnumeration [
	| expected |
	scope := ClyClassScope of: ClyClass1FromP1.
	
	scope methodsDo: [ :each | result add: each selector ].
	
	expected := ClyClass1FromP1 localMethods collect: #selector.
	self assert: result sorted asArray equals: expected sorted asArray
]

{ #category : #tests }
ClyClassScopeTest >> testVariablesEnumerationForClassWithInstanceVariables [
	scope := ClyClassScope of: ClyClass1FromP1.
	
	scope variablesDo: [ :each | result add: each ].
	
	self assert: (result as: Set) equals: {
		ClyInstanceVariable named: #instanceSideVar2 definedIn: ClyClass1FromP1.
		ClyInstanceVariable named: #instanceSideVar1 definedIn: ClyClass1FromP1
	} asSet
]

{ #category : #tests }
ClyClassScopeTest >> testVariablesEnumerationForObject [
	scope := ClyClassScope of: Object.
	
	scope variablesDo: [ :each | result add: each ].
	
	self assert: result size equals: 1.
	self
		assert: result anyOne
		equals: (ClyClassVariable on: Object classVariables first definedIn: Object)
]

{ #category : #tests }
ClyClassScopeTest >> testVariablesEnumerationForTwoClassesWithClassAndInstanceVars [
	scope := ClyClassScope ofAll: {Object. ClyClass1FromP1}.
	
	scope variablesDo: [ :each | result add: each ].
	
	self assert: (result as: Set) equals: {
		ClyClassVariable named: #DependentsFields definedIn: Object.
		ClyInstanceVariable named: #instanceSideVar2 definedIn: ClyClass1FromP1.
		ClyInstanceVariable named: #instanceSideVar1 definedIn: ClyClass1FromP1
	} asSet
]

{ #category : #tests }
ClyClassScopeTest >> testVariablesEnumerationWhenThereAreNoVariables [
	scope := ClyClassScope of: ProtoObject.
	
	scope variablesDo: [ :each | result add: each ].
	
	self assert: result isEmpty
]
