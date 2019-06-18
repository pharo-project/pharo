Class {
	#name : #ClyInstanceSideScopeTest,
	#superclass : #ClyLocalClassScopeTestCase,
	#category : #'Calypso-SystemQueries-Tests-Scopes'
}

{ #category : #running }
ClyInstanceSideScopeTest >> scopeClass [

	^ClyInstanceSideScope
]

{ #category : #tests }
ClyInstanceSideScopeTest >> testClassEnumeration [
	scope := ClyInstanceSideScope ofAll: {ClyClass1FromP1 classSide. ClyClass2FromP1}.
	
	scope classesDo: [ :each | result add: each ].
	
	self assert: result asArray equals: {ClyClass1FromP1. ClyClass2FromP1}
]

{ #category : #tests }
ClyInstanceSideScopeTest >> testMethodsEnumerationWhenBasisIsClass [
	| expected |
	scope := ClyInstanceSideScope of: ClyClass1FromP1.
	
	scope methodsDo: [ :each | result add: each selector ].
	
	expected := ClyClass1FromP1 localMethods collect: #selector.
	self assert: result sorted asArray equals: expected sorted asArray
]

{ #category : #tests }
ClyInstanceSideScopeTest >> testMethodsEnumerationWhenBasisIsMetaclass [
	| expected |
	scope := ClyInstanceSideScope of: ClyClass1FromP1 classSide.
	
	scope methodsDo: [ :each | result add: each selector ].
	
	expected := ClyClass1FromP1 localMethods collect: #selector.
	self assert: result sorted asArray equals: expected sorted asArray
]
