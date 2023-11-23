Class {
	#name : 'ClyMethodsInProtocolQueryTest',
	#superclass : 'ClyMethodQueryTest',
	#category : 'Calypso-SystemQueries-Tests-Queries',
	#package : 'Calypso-SystemQueries-Tests',
	#tag : 'Queries'
}

{ #category : 'running' }
ClyMethodsInProtocolQueryTest >> createQuery [
	^ ClyMethodsInProtocolQuery
		by: #tag1
		from: (ClyClassScope of: ClyClass1FromP1Mock in: environment)
]

{ #category : 'tests' }
ClyMethodsInProtocolQueryTest >> testCheckIfEmpty [
	query scope: (ClyMethodScope
		of: ClyClass1FromP1Mock >> #tag1Method1 in: environment).
	self deny: query checkEmptyResult.

	query scope: (ClyMethodScope of: Object >> #printString in: environment).
	self assert: query checkEmptyResult
]

{ #category : 'tests' }
ClyMethodsInProtocolQueryTest >> testComparisonToSimilarQueryButWithDifferentTag [
	| query2 |
	query tag: #tag1.
	query2 := self createQuery.
	query2 tag: #anotherTag.

	self deny: query equals: query2
]

{ #category : 'tests' }
ClyMethodsInProtocolQueryTest >> testFromThreeMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyClass1FromP1Mock >> #tag1Method1. Object >> #printString.
		ClyClass2FromP1Mock >> #instanceSideClassVar1Reader}.

	self assert: foundSelectors equals: #(tag1Method1)
]
