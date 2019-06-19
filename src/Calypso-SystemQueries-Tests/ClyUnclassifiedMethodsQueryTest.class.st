Class {
	#name : #ClyUnclassifiedMethodsQueryTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyUnclassifiedMethodsQueryTest >> createQuery [
	^ ClyUnclassifiedMethodsQuery
		from: (ClyClassScope of: ClyClass1FromP1 in: environment)
]

{ #category : #tests }
ClyUnclassifiedMethodsQueryTest >> testCheckIfEmpty [
	query scope:(ClyMethodScope
		of: ClyClass2FromP1 >> #instanceSideClassVar1Reader in: environment).
	self deny: query checkEmptyResult.

	query scope:	(ClyMethodScope of: ClyClass1FromP1 >> #tag1Method1 in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyUnclassifiedMethodsQueryTest >> testFromThreeMethods [

	self queryFromScope: ClyMethodScope ofAll: {
		ClyClass1FromP1 >> #tag1Method1. Object >> #printString. 
		ClyClass2FromP1 >> #instanceSideClassVar1Reader}.
	
	self assert: foundSelectors equals: #(instanceSideClassVar1Reader)
]
