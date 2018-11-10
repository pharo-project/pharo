Class {
	#name : #ClyAbstractMethodsTests,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries-Tests'
}

{ #category : #running }
ClyAbstractMethodsTests >> createQuery [
	^ClyAbstractMethods from: ClyClassScope of: ClyAbstractClassExample in: environment
]

{ #category : #tests }
ClyAbstractMethodsTests >> testCheckIfEmpty [
	query scope: (ClyMethodScope
		of: ClyAbstractClassExample >> #abstractMethod1 in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope of: Object >> #printString in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyAbstractMethodsTests >> testFromThreeMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyAbstractClassExample >> #abstractMethod1. 
		ClyAbstractClassExample >> #abstractMethod2.
		Object >> #printString}.
	
	self assert: foundSelectors equals: #(abstractMethod1 abstractMethod2)
]

{ #category : #tests }
ClyAbstractMethodsTests >> testNotSelectsNotAbstractMethod [
	
	self deny: (query selectsMethod: Object >> #printString)
]

{ #category : #tests }
ClyAbstractMethodsTests >> testSelectsAbstractMethod [

	self assert: (query selectsMethod: ClyAbstractClassExample>>#abstractMethod1)
]
