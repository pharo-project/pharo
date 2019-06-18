Class {
	#name : #ClyDeprecatedMethodsQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemPlugins-Deprecation-Queries-Tests'
}

{ #category : #running }
ClyDeprecatedMethodsQueryTest >> createQuery [
	^ ClyDeprecatedMethodsQuery from: ClyClassScope of: Object in: environment
]

{ #category : #tests }
ClyDeprecatedMethodsQueryTest >> testCheckIfEmpty [
	query scope: (ClyMethodScope
		of: (ClyClassWithDeprecatedMethods >> #deprecatedMethod1)
		in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope
		of: (ClyClassWithDeprecatedMethods >> #notDeprecatedMethod)
		in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyDeprecatedMethodsQueryTest >> testFromThreeMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyClassWithDeprecatedMethods >> #deprecatedMethod1. 
		ClyClassWithDeprecatedMethods >> #deprecatedMethod2.
		ClyClassWithDeprecatedMethods >> #notDeprecatedMethod}.
	
	self assert: foundSelectors sorted asArray equals: #(deprecatedMethod1 deprecatedMethod2)
]

{ #category : #tests }
ClyDeprecatedMethodsQueryTest >> testNotSelectsNotDeprecatedMethod [

	self deny: (query selectsMethod: ClyClassWithDeprecatedMethods >> #notDeprecatedMethod)
]

{ #category : #tests }
ClyDeprecatedMethodsQueryTest >> testSelectsDeprecatedMethod [

	self assert: (query selectsMethod: ClyClassWithDeprecatedMethods >> #deprecatedMethod1)
]
