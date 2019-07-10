Class {
	#name : #ClyConstantQueryTest,
	#superclass : #ClyTypedQueryTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #running }
ClyConstantQueryTest >> createQuery [
	^ ClyConstantQuery
		returning: #(item1 item2)
		from: (ClyScopeExample emptyIn: environment)
]

{ #category : #tests }
ClyConstantQueryTest >> testComparisonToSimilarQueryWhichReturningDifferentObjects [
	
	| query2 |
	query2 := self createQuery.
	query2 resultItems: #(anotherResult).
	
	self deny: query = query2
]

{ #category : #tests }
ClyConstantQueryTest >> testConvertingToAsyncQuery [

	| convertedQuery |
	convertedQuery := query async.
	
	self assert: convertedQuery == query
]

{ #category : #tests }
ClyConstantQueryTest >> testConvertingToSemiAsyncQuery [

	| convertedQuery |
	convertedQuery := query semiAsync.
	
	self assert: convertedQuery == query
]

{ #category : #tests }
ClyConstantQueryTest >> testExecution [

	self executeQuery.
	
	self assert: resultItems asArray equals: query resultItems
]
