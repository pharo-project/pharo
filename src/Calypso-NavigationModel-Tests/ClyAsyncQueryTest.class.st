Class {
	#name : #ClyAsyncQueryTest,
	#superclass : #ClyWrapQueryTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #running }
ClyAsyncQueryTest >> createQuery [
	^ ClyAsyncQuery for: (ClyQueryExampleReturningScopeBasis
			from: ClyScopeExample of: self class in: environment)
]

{ #category : #running }
ClyAsyncQueryTest >> queryClass [
	^ClyAsyncQuery 
]

{ #category : #tests }
ClyAsyncQueryTest >> testComparingWithAnotherAsyncQueryWithDifferentAsyncResult [
	| anotherQuery |
	anotherQuery := query semiAsync.

	self deny: query equals: anotherQuery
]

{ #category : #tests }
ClyAsyncQueryTest >> testConvertingToAsyncQuery [
	self assert: query async identicalTo: query
]

{ #category : #tests }
ClyAsyncQueryTest >> testConvertingToSemiAsyncQuery [
	| convertedQuery |
	convertedQuery := query semiAsync.

	self assert: convertedQuery asyncResult class equals: ClySemiAsyncQueryResult.
	self deny: convertedQuery identicalTo: query.
	self assert: convertedQuery class equals: query class.
	self assert: convertedQuery requiredResult identicalTo: query requiredResult.
	self assert: convertedQuery scope identicalTo: query scope
]

{ #category : #tests }
ClyAsyncQueryTest >> testHasAsyncResultbyDefault [

	self assert: query asyncResult class equals: ClyAsyncQueryResult
]

{ #category : #tests }
ClyAsyncQueryTest >> testPrepareResultForExecution [
	| preparedResult |
	preparedResult := query prepareNewResult.

	self assert: preparedResult class equals: ClyAsyncQueryResult.
	self assert: preparedResult buildingQuery identicalTo: query actualQuery.
	self assert: preparedResult environment identicalTo: environment.
	self assert: preparedResult isProtected
]

{ #category : #tests }
ClyAsyncQueryTest >> testPrepareResultForExecutionWhenItIsSemiAsync [
	| preparedResult |
	preparedResult := query semiAsync prepareNewResult.

	self assert: preparedResult class equals: ClySemiAsyncQueryResult.
	self assert: preparedResult buildingQuery identicalTo: query actualQuery.
	self assert: preparedResult environment identicalTo: environment.
	self assert: preparedResult isProtected
]
