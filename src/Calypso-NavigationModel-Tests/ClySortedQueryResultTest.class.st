Class {
	#name : #ClySortedQueryResultTest,
	#superclass : #ClyQueryResultTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #running }
ClySortedQueryResultTest >> createQueryResult [
	^ClySortedQueryResult using: ClySortByNameFunction ascending
]

{ #category : #tests }
ClySortedQueryResultTest >> testComparisonWithSortedResultBasedOnAnotherFunction [

	| queryResult2 |
	queryResult2 := ClySortedQueryResult using: #someAspect ascending.
	
	self deny: (queryResult = queryResult2)
]

{ #category : #tests }
ClySortedQueryResultTest >> testFillingItems [

	queryResult fillWith: {Object. Array. Point} "query sorts items by name".
	
	self assert: queryResult items asArray equals: { Array. Object. Point }
]
