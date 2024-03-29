Class {
	#name : 'ClyRawItemCursorTest',
	#superclass : 'ClyBrowserQueryCursorTest',
	#category : 'Calypso-NavigationModel-Tests',
	#package : 'Calypso-NavigationModel-Tests'
}

{ #category : 'running' }
ClyRawItemCursorTest >> createQueryResult [
	^ClyRawQueryResult new
]

{ #category : 'running' }
ClyRawItemCursorTest >> rawResultItems [

	^queryResult items
]

{ #category : 'tests' }
ClyRawItemCursorTest >> testAllItems [
	| allItems unprepared |
	allItems := cursor retrieveAll.

	self deny: allItems equals: queryResult items.
	self assert: (allItems collect: [:each | each actualObject]) equals: queryResult items.
	unprepared := allItems reject: [:each | each isPrepared].
	self assert: unprepared isEmpty.
	self assert: (allItems collect: [:each | each position]) asArray equals: (1 to: queryResult size) asArray
]

{ #category : 'tests' }
ClyRawItemCursorTest >> testHasResultAdapterOverActualQueryResult [

	self assert: cursor queryResult class equals: ClyQueryResultBrowserAdapter.
	self assert: cursor queryResult actualResult equals: queryResult
]
