Class {
	#name : #ClyFlaggingMethodsTests,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemPlugins-Flags-Queries-Tests'
}

{ #category : #running }
ClyFlaggingMethodsTests >> createQuery [
	^ClyFlaggingMethods from: ClyClassScope of: Object in: environment
]

{ #category : #tests }
ClyFlaggingMethodsTests >> testCheckIfEmpty [
	query scope: (ClyMethodScope
		of: (ClyClassWithFlags >> #methodWithFlag)
		in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope
		of: (ClyClassWithFlags >> #methodWithoutFlag)
		in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyFlaggingMethodsTests >> testFromTwoMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyClassWithFlags >> #methodWithFlag. 
		ClyClassWithFlags >> #methodWithoutFlag}.
	
	self assert: foundSelectors sorted asArray equals: #(methodWithFlag)
]

{ #category : #tests }
ClyFlaggingMethodsTests >> testNotSelectsMethodWithoutFlag [

	self deny: (query selectsMethod: ClyClassWithFlags >> #methodWithoutFlag)
]

{ #category : #tests }
ClyFlaggingMethodsTests >> testSelectsMethodWithFlag [

	self assert: (query selectsMethod: ClyClassWithFlags >> #methodWithFlag)
]
