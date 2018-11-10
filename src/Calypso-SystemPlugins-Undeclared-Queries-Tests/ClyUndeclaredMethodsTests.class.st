Class {
	#name : #ClyUndeclaredMethodsTests,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemPlugins-Undeclared-Queries-Tests'
}

{ #category : #running }
ClyUndeclaredMethodsTests >> createQuery [
	^ ClyUndeclaredMethods
		from: (ClyClassScope of: ClyClassWithUndeclares in: environment)
]

{ #category : #tests }
ClyUndeclaredMethodsTests >> testCheckIfEmpty [
	query scope: (ClyMethodScope
		of: ClyClassWithUndeclares >> #method1WithUndeclares in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope of: Object >> #printString in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyUndeclaredMethodsTests >> testFromThreeMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyClassWithUndeclares >> #method1WithUndeclares. 
		ClyClassWithUndeclares >> #method2WithUndeclares.
		Object >> #printString}.
	
	self assert: foundSelectors sorted asArray equals: #(method1WithUndeclares method2WithUndeclares) 
]

{ #category : #tests }
ClyUndeclaredMethodsTests >> testNotSelectsClassSideMethodWhichReturnesFloat [

	self deny: (query selectsMethod: ClyClassWithUndeclares class>>#withFloatReturn)
]

{ #category : #tests }
ClyUndeclaredMethodsTests >> testNotSelectsMethodWhichHasNoUndeclares [
	
	self deny: (query selectsMethod: Object >> #printString)
]

{ #category : #tests }
ClyUndeclaredMethodsTests >> testSelectsMethodWhichUsesUndeclares [
	
	self assert: (query selectsMethod: ClyClassWithUndeclares >> #method1WithUndeclares)
]
