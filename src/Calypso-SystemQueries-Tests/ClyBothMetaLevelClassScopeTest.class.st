Class {
	#name : #ClyBothMetaLevelClassScopeTest,
	#superclass : #ClyLocalClassScopeTestCase,
	#category : #'Calypso-SystemQueries-Tests-Scopes'
}

{ #category : #running }
ClyBothMetaLevelClassScopeTest >> scopeClass [

	^ClyBothMetaLevelClassScope
]

{ #category : #tests }
ClyBothMetaLevelClassScopeTest >> testClassEnumeration [
	scope := ClyBothMetaLevelClassScope ofAll: {ClyClass1FromP1Mock. ClyClass2FromP1Mock}.
	
	scope classesDo: [ :each | result add: each ].
	
	self assert: result asArray equals: {ClyClass1FromP1Mock. ClyClass2FromP1Mock}
]

{ #category : #tests }
ClyBothMetaLevelClassScopeTest >> testClassEnumerationWhenMetaClassIsBasis [
	scope := ClyBothMetaLevelClassScope of: ClyClass1FromP1Mock class.
	
	scope classesDo: [ :each | result add: each ].
	
	self assert: result asArray equals: {ClyClass1FromP1Mock}
]

{ #category : #tests }
ClyBothMetaLevelClassScopeTest >> testMethodsEnumerationWhenBasisIsClass [
	scope := ClyBothMetaLevelClassScope of: ClyClass1FromP1Mock.
	
	scope methodsDo: [ :each | result add: each ].
	
	self assert: (result includesAll: ClyClass1FromP1Mock localMethods).
	self assert: (result includesAll: ClyClass1FromP1Mock classSide localMethods)
]

{ #category : #tests }
ClyBothMetaLevelClassScopeTest >> testMethodsEnumerationWhenBasisIsMetaclass [
	scope := ClyBothMetaLevelClassScope of: ClyClass1FromP1Mock classSide.
	
	scope methodsDo: [ :each | result add: each ].
	
	self assert: (result includesAll: ClyClass1FromP1Mock localMethods).
	self assert: (result includesAll: ClyClass1FromP1Mock classSide localMethods)
]
