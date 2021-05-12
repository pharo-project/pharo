Class {
	#name : #ClyUntaggedClassesQueryTest,
	#superclass : #ClyClassQueryTest,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyUntaggedClassesQueryTest >> createQuery [
	^ ClyUntaggedClassesQuery
		from: (ClyClassScope of: Object in: environment)
]

{ #category : #tests }
ClyUntaggedClassesQueryTest >> testCheckIfEmpty [
	query scope: (ClyClassScope of: ClyClass6UntaggedFromP5Mock in: environment).
	self deny: query checkEmptyResult.
	
	query scope: (ClyClassScope of: ClyClass7WithTag1FromP5Mock in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyUntaggedClassesQueryTest >> testFromThreeClasses [
	
	self queryFromScope: ClyClassScope ofAll: {
		ClyClass7WithTag1FromP5Mock. ClyClass6UntaggedFromP5Mock. ClyClass1FromP1Mock }.

	self assert: resultItems asSet equals: {ClyClass6UntaggedFromP5Mock. ClyClass1FromP1Mock} asSet
]

{ #category : #tests }
ClyUntaggedClassesQueryTest >> testNotSelectsTaggedClass [
	
	self deny: (query selectsClass: ClyClass7WithTag1FromP5Mock)
]

{ #category : #tests }
ClyUntaggedClassesQueryTest >> testSelectsClassWithoutTags [
	
	self assert: (query selectsClass: ClyClass6UntaggedFromP5Mock)
]
