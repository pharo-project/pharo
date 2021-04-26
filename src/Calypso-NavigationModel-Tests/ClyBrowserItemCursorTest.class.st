Class {
	#name : #ClyBrowserItemCursorTest,
	#superclass : #ClyBrowserQueryCursorTest,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #running }
ClyBrowserItemCursorTest >> createQueryResult [
	^ClyBrowserQueryExampleResult new
]

{ #category : #running }
ClyBrowserItemCursorTest >> rawResultItems [
	^queryResult items collect: [ :each | each actualObject ]
]

{ #category : #tests }
ClyBrowserItemCursorTest >> testAllItems [
	
	| allItems unprepared |
	allItems := cursor retrieveAll.
	
	self assert: allItems equals: queryResult items.
	unprepared := allItems reject: [:each | each isPrepared].
	self assert: unprepared isEmpty.
	self assert: (allItems collect: [:each | each position]) asArray equals: (1 to: queryResult size) asArray.
]

{ #category : #tests }
ClyBrowserItemCursorTest >> testHasQueryResult [

	self assert: cursor queryResult equals: queryResult
]
