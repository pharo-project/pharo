Class {
	#name : #ClyRawItemCursorTests,
	#superclass : #ClyBrowserQueryCursorTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #running }
ClyRawItemCursorTests >> createQueryResult [
	^ClyRawQueryResult new
]

{ #category : #running }
ClyRawItemCursorTests >> rawResultItems [

	^queryResult items
]

{ #category : #tests }
ClyRawItemCursorTests >> testAllItems [
	
	| allItems unprepared |
	allItems := cursor retrieveAll.
	
	self deny: allItems = queryResult items.
	self assert: (allItems collect: #actualObject) equals: queryResult items.
	unprepared := allItems reject: #isPrepared.
	self assert: unprepared isEmpty.
	self assert: (allItems collect: #position) asArray equals: (1 to: queryResult size) asArray.
]

{ #category : #tests }
ClyRawItemCursorTests >> testHasResultAdapterOverActualQueryResult [

	self assert: cursor queryResult class equals: ClyQueryResultBrowserAdapter.
	self assert: cursor queryResult actualResult equals: queryResult
]
