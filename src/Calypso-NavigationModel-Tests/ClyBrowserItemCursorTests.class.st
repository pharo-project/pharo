Class {
	#name : #ClyBrowserItemCursorTests,
	#superclass : #ClyBrowserQueryCursorTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #running }
ClyBrowserItemCursorTests >> createQueryResult [
	^ClyBrowserQueryResultExample new
]

{ #category : #running }
ClyBrowserItemCursorTests >> rawResultItems [
	^queryResult items collect: #actualObject
]

{ #category : #tests }
ClyBrowserItemCursorTests >> testAllItems [
	
	| allItems unprepared |
	allItems := cursor retrieveAll.
	
	self assert: allItems equals: queryResult items.
	unprepared := allItems reject: #isPrepared.
	self assert: unprepared isEmpty.
	self assert: (allItems collect: #position) asArray equals: (1 to: queryResult size) asArray.
]

{ #category : #tests }
ClyBrowserItemCursorTests >> testHasQueryResult [

	self assert: cursor queryResult equals: queryResult
]
