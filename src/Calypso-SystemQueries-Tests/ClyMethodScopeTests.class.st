Class {
	#name : #ClyMethodScopeTests,
	#superclass : #ClyTypedScopeTestCase,
	#category : #'Calypso-SystemQueries-Tests-Scopes'
}

{ #category : #running }
ClyMethodScopeTests >> extraBasisSample [
	^self class >> #method2
]

{ #category : #helpers }
ClyMethodScopeTests >> method1 [
]

{ #category : #helpers }
ClyMethodScopeTests >> method2 [
]

{ #category : #running }
ClyMethodScopeTests >> scopeClass [

	^ClyMethodScope
]

{ #category : #running }
ClyMethodScopeTests >> singleBasisSample [
	^self class >> #method1
]

{ #category : #tests }
ClyMethodScopeTests >> testMethodsEnumeration [
	scope := ClyMethodScope ofAll: {self class >> #method1. self class >> #method2}.
		
	scope methodsDo: [ :each | result add: each ].
	
	self assert: result asArray equals: scope basisObjects asArray
]
