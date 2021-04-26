Class {
	#name : #ClyBrowserQueryCursorTest,
	#superclass : #ClyAbsractNavigationEnvironmentTest,
	#instVars : [
		'cursor',
		'queryResult',
		'itemObserver'
	],
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyBrowserQueryCursorTest class >> isAbstract [
	^self = ClyBrowserQueryCursorTest 
]

{ #category : #running }
ClyBrowserQueryCursorTest >> createQuery [
	^ClyReturningScopeBasisExampleQuery
		from: (ClyExampleScope ofAll: {Array. Object. String} in: environment)
		as: self createQueryResult
]

{ #category : #running }
ClyBrowserQueryCursorTest >> createQueryResult [
	self subclassResponsibility 
]

{ #category : #running }
ClyBrowserQueryCursorTest >> rawResultItems [
	self subclassResponsibility
]

{ #category : #running }
ClyBrowserQueryCursorTest >> setUp [
	| query |
	super setUp.
	query := self createQuery.
	queryResult := query execute.
	itemObserver := ClyItemObserverExample new.
	cursor := queryResult openBrowserCursorFor: itemObserver
]

{ #category : #tests }
ClyBrowserQueryCursorTest >> testClosing [

	cursor close.
	self deny: (queryResult hasSubscriber: itemObserver)
]

{ #category : #tests }
ClyBrowserQueryCursorTest >> testCurrentItem [
	
	| item |
	item := cursor currentItem.
	
	self assert: item class equals: ClyBrowserItem.
	self assert: item isPrepared.
	self assert: item position equals: 1.
]

{ #category : #tests }
ClyBrowserQueryCursorTest >> testFindItemsSimilarToOthers [
	
	| found |
	found := cursor findItemsSimilarTo: ({Class. Array} collect: [:each | each asCalypsoBrowserItem]).
	
	self assert: found size equals: 1.
	self assert: found first actualObject equals: Array.
	self assert: found first isPrepared.
	self assert: found first position equals: (self rawResultItems indexOf: Array).
]

{ #category : #tests }
ClyBrowserQueryCursorTest >> testFindItemsSimilarToOthersWhenTheyAreNotInCache [
	
	| found |
	cursor cleanCache.
	found := cursor findItemsSimilarTo: ({Class. Array} collect: [:each | each asCalypsoBrowserItem]).
	
	self assert: found size equals: 1.
	self assert: found first actualObject equals: Array.
	self assert: found first isPrepared.
	self assert: found first position equals: (self rawResultItems indexOf: Array).
]

{ #category : #tests }
ClyBrowserQueryCursorTest >> testFindItemsUsingPredicate [
	
	| allItems |
	allItems := cursor findItemsWhere: [:each | each actualObject = Array].
	
	self assert: allItems size equals: 1.
	self assert: allItems first actualObject equals: Array.
	self assert: allItems first isPrepared.
	self assert: allItems first position equals: (self rawResultItems indexOf: Array).
]

{ #category : #tests }
ClyBrowserQueryCursorTest >> testFindItemsUsingPredicateWhenTheyAreNotInCache [
	
	| allItems |
	cursor cleanCache.
	allItems := cursor findItemsWhere: [:each | each actualObject = Object].
	
	self assert: allItems size equals: 1.
	self assert: allItems first actualObject equals: Object.
	self assert: allItems first isPrepared.
	self assert: allItems first position equals: 2.
]

{ #category : #tests }
ClyBrowserQueryCursorTest >> testFindItemsWithActualObjects [
	
	| found |
	found := cursor findItemsWith: {#absentItem1. Array. #absentItem2}.
	
	self assert: found size equals: 3.
	self assert: found first equals: nil.
	self assert: found last equals: nil.
	self assert: found second actualObject equals: Array.
	self assert: found second isPrepared.
	self assert: found second position equals: (self rawResultItems indexOf: Array).
]

{ #category : #tests }
ClyBrowserQueryCursorTest >> testFindItemsWithActualObjectsWhenTheyAreNotInCache [
	
	| found |
	cursor cleanCache.
	found := cursor findItemsWith: {#absentItem1. Array. #absentItem2}.
	
	self assert: found size equals: 3.
	self assert: found first equals: nil.
	self assert: found last equals: nil.
	self assert: found second actualObject equals: Array.
	self assert: found second isPrepared.
	self assert: found second position equals: (self rawResultItems indexOf: Array).
]

{ #category : #tests }
ClyBrowserQueryCursorTest >> testMoveToItemWhichNotSatisfiesCondition [
	
	| found |
	found := cursor moveToItemWhich: [:each | each actualObject = Object].
	self assert: found.
	self assert: cursor position equals: 2.
	
	found := cursor moveToItemWhich: [:each | false].
	self deny: found.	
	self assert: cursor position equals: 2
]

{ #category : #tests }
ClyBrowserQueryCursorTest >> testMoveToItemWhichSatisfiesCondition [
	
	| found |
	found := cursor moveToItemWhich: [:each | each actualObject = Object].
	
	self assert: found.	
	self assert: cursor position equals: 2.
	self assert: cursor currentItem actualObject equals: Object.
]

{ #category : #tests }
ClyBrowserQueryCursorTest >> testMoveToItemWhichSatisfiesConditionWhenItNotExistsInCache [
	
	| found |
	cursor cleanCache.
	found := cursor moveToItemWhich: [:each | each actualObject = Object].
	
	self assert: found.	
	self assert: cursor position equals: 2.
	self assert: cursor currentItem actualObject equals: Object.
]

{ #category : #tests }
ClyBrowserQueryCursorTest >> testOpeningShouldSubscribeObserver [

	self assert: (queryResult hasSubscriber: itemObserver).
	self assert: cursor itemObserver equals: itemObserver
]
