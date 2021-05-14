Class {
	#name : #ClyAllExtensionMethodsQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyAllExtensionMethodsQueryTest >> createQuery [
	^ClyAllExtensionMethodsQuery from: ClyClassScope of: Object in: environment
]

{ #category : #tests }
ClyAllExtensionMethodsQueryTest >> testCheckIfEmpty [
	query scope: (ClyMethodScope
		of: (ClySubclassLOfClass0FromPExtendedByP1Mock>>#extensionMethodFromSubclassL)
		in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope of: (ClyClass1FromP1Mock >> #tag1Method1) in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyAllExtensionMethodsQueryTest >> testFromThreeMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyClass1FromP1Mock >> #tag1Method1. Object >> #printString. 
		ClySubclassLOfClass0FromPExtendedByP1Mock >> #extensionMethodFromSubclassL}.
	
	self assert: foundSelectors equals: #(#extensionMethodFromSubclassL)
]
