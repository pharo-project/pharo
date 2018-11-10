Class {
	#name : #ClyAllExtensionMethodsTests,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyAllExtensionMethodsTests >> createQuery [
	^ClyAllExtensionMethods from: ClyClassScope of: Object in: environment
]

{ #category : #tests }
ClyAllExtensionMethodsTests >> testCheckIfEmpty [
	query scope: (ClyMethodScope
		of: (ClySubclassLOfClass0FromPExtendedByP1>>#extensionMethodFromSubclassL)
		in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope of: (ClyClass1FromP1 >> #tag1Method1) in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyAllExtensionMethodsTests >> testFromThreeMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyClass1FromP1 >> #tag1Method1. Object >> #printString. 
		ClySubclassLOfClass0FromPExtendedByP1 >> #extensionMethodFromSubclassL}.
	
	self assert: foundSelectors equals: #(#extensionMethodFromSubclassL)
]
