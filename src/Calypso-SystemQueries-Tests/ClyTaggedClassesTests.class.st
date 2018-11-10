Class {
	#name : #ClyTaggedClassesTests,
	#superclass : #ClyClassQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyTaggedClassesTests >> createQuery [
	^ ClyTaggedClasses
		by: #Tag1
		from: (ClyClassScope of: Object in: environment)
]

{ #category : #tests }
ClyTaggedClassesTests >> testCheckIfEmpty [
	query tag: #Tag1.
	query scope: (ClyClassScope of: ClyClass7WithTag1FromP5 in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyClassScope of: ClyClass8WithTag2FromP5 in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyTaggedClassesTests >> testComparisonToSimilarQueryWithDifferentTag [
	
	| query2 |
	query2 := self createQuery.
	query2 tag: #AnotherTag1.
	
	self deny: query = query2
]

{ #category : #tests }
ClyTaggedClassesTests >> testFromThreeClasses [
	query tag: #Tag1.
	
	self queryFromScope: ClyClassScope ofAll: {
		ClyClass6UntaggedFromP5. ClyClass7WithTag1FromP5. ClyClass8WithTag2FromP5 }.

	self assert: resultItems asArray equals: {ClyClass7WithTag1FromP5}
]
