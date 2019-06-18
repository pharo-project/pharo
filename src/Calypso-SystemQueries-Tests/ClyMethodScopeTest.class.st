Class {
	#name : #ClyMethodScopeTest,
	#superclass : #ClyTypedScopeTestCase,
	#category : #'Calypso-SystemQueries-Tests-Scopes'
}

{ #category : #running }
ClyMethodScopeTest >> extraBasisSample [
	^self class >> #method2
]

{ #category : #helpers }
ClyMethodScopeTest >> method1 [
]

{ #category : #helpers }
ClyMethodScopeTest >> method2 [
]

{ #category : #running }
ClyMethodScopeTest >> scopeClass [

	^ClyMethodScope
]

{ #category : #running }
ClyMethodScopeTest >> singleBasisSample [
	^self class >> #method1
]

{ #category : #tests }
ClyMethodScopeTest >> testMethodsEnumeration [
	scope := ClyMethodScope ofAll: {self class >> #method1. self class >> #method2}.
		
	scope methodsDo: [ :each | result add: each ].
	
	self assert: result asArray equals: scope basisObjects asArray
]
