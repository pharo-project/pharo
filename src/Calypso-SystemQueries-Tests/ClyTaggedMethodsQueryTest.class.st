Class {
	#name : #ClyTaggedMethodsQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyTaggedMethodsQueryTest >> createQuery [
	^ ClyTaggedMethodsQuery
		by: #tag1
		from: (ClyClassScope of: ClyClass1FromP1 in: environment)
]

{ #category : #tests }
ClyTaggedMethodsQueryTest >> testCheckIfEmpty [
	query scope: (ClyMethodScope
		of: ClyClass1FromP1 >> #tag1Method1 in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyMethodScope of: Object >> #printString in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyTaggedMethodsQueryTest >> testComparisonToSimilarQueryButWithDifferentTag [

	| query2 |
	query tag: #tag1.
	query2 := self createQuery.
	query2 tag: #anotherTag.
	
	self deny: query = query2
]

{ #category : #tests }
ClyTaggedMethodsQueryTest >> testFromThreeMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyClass1FromP1 >> #tag1Method1. Object >> #printString. 
		ClyClass2FromP1 >> #instanceSideClassVar1Reader}.
	
	self assert: foundSelectors equals: #(tag1Method1)
]
