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
	scope := ClyInstanceSideScope ofAll: {ClyClass1FromP1Mock classSide. ClyClass2FromP1Mock}.
	
	scope classesDo: [ :each | result add: each ].
	
	self assert: result asArray equals: {ClyClass1FromP1Mock. ClyClass2FromP1Mock}
]

{ #category : #tests }
ClyInstanceSideScopeTest >> testMethodsEnumerationWhenBasisIsClass [

	| expected |
	scope := ClyInstanceSideScope of: ClyClass1FromP1Mock.
	scope methodsDo: [ :each | result add: each selector ].
	expected := ClyClass1FromP1Mock localMethods collect: [:each | each selector].
	self assert: result sorted asArray equals: expected sorted asArray
]

{ #category : #tests }
ClyInstanceSideScopeTest >> testMethodsEnumerationWhenBasisIsMetaclass [

	| expected |
	scope := ClyInstanceSideScope of: ClyClass1FromP1Mock classSide.	
	scope methodsDo: [ :each | result add: each selector ].
	expected := ClyClass1FromP1Mock localMethods collect: [:each | each selector].
	self assert: result sorted asArray equals: expected sorted asArray
]

{ #category : #tests }
ClyInstanceSideScopeTest >> testMethodsEnumerationWhenBasisIsTraitedClass [

	| expected |
	scope := ClyInstanceSideScope of: ClyClassWithTraits.	
	scope methodsDo: [ :each | result add: each selector ].
	expected := ClyClassWithTraits methods collect: [:each | each selector].
	self assert: result sorted asArray equals: expected sorted asArray
]

{ #category : #tests }
ClyInstanceSideScopeTest >> testMethodsEnumerationWhenBasisIsTraitedClassClasSide [

	| expected |
	scope := ClyInstanceSideScope of: ClyClassWithTraits classSide.
	scope methodsDo: [ :each | result add: each selector ].
	expected := ClyClassWithTraits methods collect: [:each | each selector].
	self assert: result sorted asArray equals: expected sorted asArray
]
