Class {
	#name : #ClyRawQueryResultTest,
	#superclass : #ClyQueryResultTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #running }
ClyRawQueryResultTest >> createQueryResult [
	^ClyRawQueryResult new
]

{ #category : #tests }
ClyRawQueryResultTest >> testFillingWithItems [

	queryResult fillWith: #(1 2 3).
	
	self assert: queryResult items asArray equals: #(1 2 3)
]

{ #category : #tests }
ClyRawQueryResultTest >> testFillingWithItemsTwice [

	queryResult fillWith: #(1 2 3).
	queryResult fillWith: #(4 5 6).
	
	self assert: queryResult items asArray equals: #(1 2 3 4 5 6)
]

{ #category : #tests }
ClyRawQueryResultTest >> testGettingAllItems [

	| actual |
	queryResult fillWith: #(1 2 3).

	actual := queryResult allItems.
	
	self assert: actual asArray equals: #(1 2 3)
]

{ #category : #tests }
ClyRawQueryResultTest >> testGettingAllItemsSatisfyingCondition [

	| actual |
	queryResult fillWith: #(1 2 3 4 5 6).

	actual := queryResult itemsWhere: [:each | each between: 2 and: 4].
	
	self assert: actual asArray equals: #(2 3 4)
]

{ #category : #tests }
ClyRawQueryResultTest >> testGettingItemsStartingAtGivenPosition [

	| actual |
	queryResult fillWith: #(1 2 3 4 5 6).

	actual := queryResult itemsStartingAt: 3 count: 2.
	
	self assert: actual asArray equals: #(3 4)
]

{ #category : #tests }
ClyRawQueryResultTest >> testGettingItemsStartingWhereConditionIsValid [

	| actual |
	queryResult fillWith: #(1 2 3 4 5 6).

	actual := queryResult itemsStartingWhere: [:each | each > 3] count: 2.
	
	self assert: actual asArray equals: #(4 5)
]
