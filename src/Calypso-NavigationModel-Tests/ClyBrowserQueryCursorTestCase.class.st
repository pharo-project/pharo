Class {
	#name : #ClyBrowserQueryCursorTestCase,
	#superclass : #ClyNavigationEnvironmentTestCase,
	#instVars : [
		'cursor',
		'queryResult',
		'itemObserver'
	],
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyBrowserQueryCursorTestCase class >> isAbstract [
	^self = ClyBrowserQueryCursorTestCase 
]

{ #category : #running }
ClyBrowserQueryCursorTestCase >> createQuery [
	^ClyQueryExampleReturningScopeBasis
		from: (ClyScopeExample ofAll: {Array. Object. String} in: environment)
		as: self createQueryResult
]

{ #category : #running }
ClyBrowserQueryCursorTestCase >> createQueryResult [
	self subclassResponsibility 
]

{ #category : #running }
ClyBrowserQueryCursorTestCase >> setUp [
	| query |
	super setUp.
	query := self createQuery.
	queryResult := query execute.
	itemObserver := ClyItemObserverExample new.
	cursor := queryResult openBrowserCursorFor: itemObserver
]

{ #category : #tests }
ClyBrowserQueryCursorTestCase >> testClosing [

	cursor close.
	self deny: (queryResult hasSubscriber: itemObserver)
]

{ #category : #tests }
ClyBrowserQueryCursorTestCase >> testCurrentItem [
	
	| item |
	item := cursor currentItem.
	
	self assert: item class equals: ClyBrowserItem.
	self assert: item isPrepared.
	self assert: item position equals: 1.
]

{ #category : #tests }
ClyBrowserQueryCursorTestCase >> testFindItemsSimilarToOthers [
	
	| found |
	found := cursor findItemsSimilarTo: ({Class. Array} collect: #asCalypsoBrowserItem).
	
	self assert: found size equals: 1.
	self assert: found first actualObject equals: Array.
	self assert: found first isPrepared.
	self assert: found first position equals: (self rawResultItems indexOf: Array).
]

{ #category : #tests }
ClyBrowserQueryCursorTestCase >> testFindItemsSimilarToOthersWhenTheyAreNotInCache [
	
	| found |
	cursor cleanCache.
	found := cursor findItemsSimilarTo: ({Class. Array} collect: #asCalypsoBrowserItem).
	
	self assert: found size equals: 1.
	self assert: found first actualObject equals: Array.
	self assert: found first isPrepared.
	self assert: found first position equals: (self rawResultItems indexOf: Array).
]

{ #category : #tests }
ClyBrowserQueryCursorTestCase >> testFindItemsUsingPredicate [
	
	| allItems |
	allItems := cursor findItemsWhere: [:each | each actualObject = Array].
	
	self assert: allItems size equals: 1.
	self assert: allItems first actualObject equals: Array.
	self assert: allItems first isPrepared.
	self assert: allItems first position equals: (self rawResultItems indexOf: Array).
]

{ #category : #tests }
ClyBrowserQueryCursorTestCase >> testFindItemsUsingPredicateWhenTheyAreNotInCache [
	
	| allItems |
	cursor cleanCache.
	allItems := cursor findItemsWhere: [:each | each actualObject = Object].
	
	self assert: allItems size equals: 1.
	self assert: allItems first actualObject equals: Object.
	self assert: allItems first isPrepared.
	self assert: allItems first position equals: 2.
]

{ #category : #tests }
ClyBrowserQueryCursorTestCase >> testFindItemsWithActualObjects [
	
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
ClyBrowserQueryCursorTestCase >> testFindItemsWithActualObjectsWhenTheyAreNotInCache [
	
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
ClyBrowserQueryCursorTestCase >> testMoveToItemWhichNotSatisfiesCondition [
	
	| found |
	found := cursor moveToItemWhich: [:each | each actualObject = Object].
	self assert: found.
	self assert: cursor position equals: 2.
	
	found := cursor moveToItemWhich: [:each | false].
	self deny: found.	
	self assert: cursor position equals: 2
]

{ #category : #tests }
ClyBrowserQueryCursorTestCase >> testMoveToItemWhichSatisfiesCondition [
	
	| found |
	found := cursor moveToItemWhich: [:each | each actualObject = Object].
	
	self assert: found.	
	self assert: cursor position equals: 2.
	self assert: cursor currentItem actualObject equals: Object.
]

{ #category : #tests }
ClyBrowserQueryCursorTestCase >> testMoveToItemWhichSatisfiesConditionWhenItNotExistsInCache [
	
	| found |
	cursor cleanCache.
	found := cursor moveToItemWhich: [:each | each actualObject = Object].
	
	self assert: found.	
	self assert: cursor position equals: 2.
	self assert: cursor currentItem actualObject equals: Object.
]

{ #category : #tests }
ClyBrowserQueryCursorTestCase >> testOpeningShouldSubscribeObserver [

	self assert: (queryResult hasSubscriber: itemObserver).
	self assert: cursor itemObserver equals: itemObserver
]
