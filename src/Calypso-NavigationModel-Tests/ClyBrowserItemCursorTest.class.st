Class {
	#name : #ClyBrowserItemCursorTest,
	#superclass : #ClyBrowserQueryCursorTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #running }
ClyBrowserItemCursorTest >> createQueryResult [
	^ClyBrowserQueryResultExample new
]

{ #category : #running }
ClyBrowserItemCursorTest >> rawResultItems [
	^queryResult items collect: #actualObject
]

{ #category : #tests }
ClyBrowserItemCursorTest >> testAllItems [
	
	| allItems unprepared |
	allItems := cursor retrieveAll.
	
	self assert: allItems equals: queryResult items.
	unprepared := allItems reject: #isPrepared.
	self assert: unprepared isEmpty.
	self assert: (allItems collect: #position) asArray equals: (1 to: queryResult size) asArray.
]

{ #category : #tests }
ClyBrowserItemCursorTest >> testHasQueryResult [

	self assert: cursor queryResult equals: queryResult
]
