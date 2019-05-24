Class {
	#name : #ClyFlaggingMethodsQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemPlugins-Flags-Queries-Tests'
}

{ #category : #running }
ClyFlaggingMethodsQueryTest >> createQuery [
	^ClyFlaggingMethodsQuery from: ClyClassScope of: Object in: environment
]

{ #category : #tests }
ClyFlaggingMethodsQueryTest >> testCheckIfEmpty [
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
ClyFlaggingMethodsQueryTest >> testFromTwoMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyClassWithFlags >> #methodWithFlag. 
		ClyClassWithFlags >> #methodWithoutFlag}.
	
	self assert: foundSelectors sorted asArray equals: #(methodWithFlag)
]

{ #category : #tests }
ClyFlaggingMethodsQueryTest >> testNotSelectsMethodWithoutFlag [

	self deny: (query selectsMethod: ClyClassWithFlags >> #methodWithoutFlag)
]

{ #category : #tests }
ClyFlaggingMethodsQueryTest >> testSelectsMethodWithFlag [

	self assert: (query selectsMethod: ClyClassWithFlags >> #methodWithFlag)
]
