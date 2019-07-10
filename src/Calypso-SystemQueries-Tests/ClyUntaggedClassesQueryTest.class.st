Class {
	#name : #ClyUntaggedClassesQueryTest,
	#superclass : #ClyClassQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyUntaggedClassesQueryTest >> createQuery [
	^ ClyUntaggedClassesQuery
		from: (ClyClassScope of: Object in: environment)
]

{ #category : #tests }
ClyUntaggedClassesQueryTest >> testCheckIfEmpty [
	query scope: (ClyClassScope of: ClyClass6UntaggedFromP5 in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyClassScope of: ClyClass7WithTag1FromP5 in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyUntaggedClassesQueryTest >> testFromThreeClasses [
	
	self queryFromScope: ClyClassScope ofAll: {
		ClyClass7WithTag1FromP5. ClyClass6UntaggedFromP5. ClyClass1FromP1 }.

	self assert: resultItems asSet equals: {ClyClass6UntaggedFromP5. ClyClass1FromP1} asSet
]

{ #category : #tests }
ClyUntaggedClassesQueryTest >> testNotSelectsTaggedClass [
	
	self deny: (query selectsClass: ClyClass7WithTag1FromP5)
]

{ #category : #tests }
ClyUntaggedClassesQueryTest >> testSelectsClassWithoutTags [
	
	self assert: (query selectsClass: ClyClass6UntaggedFromP5)
]
