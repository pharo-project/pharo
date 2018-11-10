Class {
	#name : #ClyBothMetaLevelClassScopeTests,
	#superclass : #ClyLocalClassScopeTestCase,
	#category : #'Calypso-SystemQueries-Tests-Scopes'
}

{ #category : #running }
ClyBothMetaLevelClassScopeTests >> scopeClass [

	^ClyBothMetaLevelClassScope
]

{ #category : #tests }
ClyBothMetaLevelClassScopeTests >> testClassEnumeration [
	scope := ClyBothMetaLevelClassScope ofAll: {ClyClass1FromP1. ClyClass2FromP1}.
	
	scope classesDo: [ :each | result add: each ].
	
	self assert: result asArray equals: {ClyClass1FromP1. ClyClass2FromP1}
]

{ #category : #tests }
ClyBothMetaLevelClassScopeTests >> testClassEnumerationWhenMetaClassIsBasis [
	scope := ClyBothMetaLevelClassScope of: ClyClass1FromP1 class.
	
	scope classesDo: [ :each | result add: each ].
	
	self assert: result asArray equals: {ClyClass1FromP1}
]

{ #category : #tests }
ClyBothMetaLevelClassScopeTests >> testMethodsEnumerationWhenBasisIsClass [
	scope := ClyBothMetaLevelClassScope of: ClyClass1FromP1.
	
	scope methodsDo: [ :each | result add: each ].
	
	self assert: (result includesAll: ClyClass1FromP1 localMethods).
	self assert: (result includesAll: ClyClass1FromP1 classSide localMethods)
]

{ #category : #tests }
ClyBothMetaLevelClassScopeTests >> testMethodsEnumerationWhenBasisIsMetaclass [
	scope := ClyBothMetaLevelClassScope of: ClyClass1FromP1 classSide.
	
	scope methodsDo: [ :each | result add: each ].
	
	self assert: (result includesAll: ClyClass1FromP1 localMethods).
	self assert: (result includesAll: ClyClass1FromP1 classSide localMethods)
]
