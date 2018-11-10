Class {
	#name : #ClyConstantQueryTests,
	#superclass : #ClyTypedQueryTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #running }
ClyConstantQueryTests >> createQuery [
	^ ClyConstantQuery
		returning: #(item1 item2)
		from: (ClyScopeExample emptyIn: environment)
]

{ #category : #tests }
ClyConstantQueryTests >> testComparisonToSimilarQueryWhichReturningDifferentObjects [
	
	| query2 |
	query2 := self createQuery.
	query2 resultItems: #(anotherResult).
	
	self deny: query = query2
]

{ #category : #tests }
ClyConstantQueryTests >> testConvertingToAsyncQuery [

	| convertedQuery |
	convertedQuery := query async.
	
	self assert: convertedQuery == query
]

{ #category : #tests }
ClyConstantQueryTests >> testConvertingToSemiAsyncQuery [

	| convertedQuery |
	convertedQuery := query semiAsync.
	
	self assert: convertedQuery == query
]

{ #category : #tests }
ClyConstantQueryTests >> testExecution [

	self executeQuery.
	
	self assert: resultItems asArray equals: query resultItems
]
