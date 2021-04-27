Class {
	#name : #ClyTaggedClassesQueryTest,
	#superclass : #ClyClassQueryTest,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyTaggedClassesQueryTest >> createQuery [
	^ ClyTaggedClassesQuery
		by: #Tag1
		from: (ClyClassScope of: Object in: environment)
]

{ #category : #tests }
ClyTaggedClassesQueryTest >> testCheckIfEmpty [
	query tag: #Tag1.
	query scope: (ClyClassScope of: ClyClass7WithTag1FromP5Mock in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyClassScope of: ClyClass8WithTag2FromP5Mock in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyTaggedClassesQueryTest >> testComparisonToSimilarQueryWithDifferentTag [
	| query2 |
	query2 := self createQuery.
	query2 tag: #AnotherTag1.

	self deny: query equals: query2
]

{ #category : #tests }
ClyTaggedClassesQueryTest >> testFromThreeClasses [
	query tag: #Tag1.
	
	self queryFromScope: ClyClassScope ofAll: {
		ClyClass6UntaggedFromP5Mock. ClyClass7WithTag1FromP5Mock. ClyClass8WithTag2FromP5Mock }.

	self assert: resultItems asArray equals: {ClyClass7WithTag1FromP5Mock}
]
