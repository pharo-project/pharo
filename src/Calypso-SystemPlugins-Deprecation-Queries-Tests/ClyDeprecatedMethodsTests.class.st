Class {
	#name : #ClyDeprecatedMethodsTests,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemPlugins-Deprecation-Queries-Tests'
}

{ #category : #running }
ClyDeprecatedMethodsTests >> createQuery [
	^ ClyDeprecatedMethods from: ClyClassScope of: Object in: environment
]

{ #category : #tests }
ClyDeprecatedMethodsTests >> testCheckIfEmpty [
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
ClyDeprecatedMethodsTests >> testFromThreeMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyClassWithDeprecatedMethods >> #deprecatedMethod1. 
		ClyClassWithDeprecatedMethods >> #deprecatedMethod2.
		ClyClassWithDeprecatedMethods >> #notDeprecatedMethod}.
	
	self assert: foundSelectors sorted asArray equals: #(deprecatedMethod1 deprecatedMethod2)
]

{ #category : #tests }
ClyDeprecatedMethodsTests >> testNotSelectsNotDeprecatedMethod [

	self deny: (query selectsMethod: ClyClassWithDeprecatedMethods >> #notDeprecatedMethod)
]

{ #category : #tests }
ClyDeprecatedMethodsTests >> testSelectsDeprecatedMethod [

	self assert: (query selectsMethod: ClyClassWithDeprecatedMethods >> #deprecatedMethod1)
]
