Class {
	#name : #ClyAsyncQueryTests,
	#superclass : #ClyWrapQueryTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #running }
ClyAsyncQueryTests >> createQuery [
	^ ClyAsyncQuery for: (ClyQueryExampleReturningScopeBasis
			from: ClyScopeExample of: self class in: environment)
]

{ #category : #running }
ClyAsyncQueryTests >> queryClass [
	^ClyAsyncQuery 
]

{ #category : #tests }
ClyAsyncQueryTests >> testComparingWithAnotherAsyncQueryWithDifferentAsyncResult [

	| anotherQuery |
	anotherQuery := query semiAsync.
	
	self deny: query = anotherQuery
]

{ #category : #tests }
ClyAsyncQueryTests >> testConvertingToAsyncQuery [

	self assert: query async == query
	
]

{ #category : #tests }
ClyAsyncQueryTests >> testConvertingToSemiAsyncQuery [

	| convertedQuery |
	convertedQuery := query semiAsync.
	
	self assert: convertedQuery asyncResult class equals: ClySemiAsyncQueryResult.
	self deny: convertedQuery == query.
	self assert: convertedQuery class equals: query class.
	self assert: convertedQuery requiredResult == query requiredResult.
	self assert: convertedQuery scope == query scope
]

{ #category : #tests }
ClyAsyncQueryTests >> testHasAsyncResultbyDefault [

	self assert: query asyncResult class equals: ClyAsyncQueryResult
]

{ #category : #tests }
ClyAsyncQueryTests >> testPrepareResultForExecution [

	| preparedResult |
	preparedResult := query prepareNewResult.
	
	self assert: preparedResult class equals: ClyAsyncQueryResult.
	self assert: preparedResult buildingQuery == query actualQuery.
	self assert: preparedResult environment == environment.
	self assert: preparedResult isProtected
]

{ #category : #tests }
ClyAsyncQueryTests >> testPrepareResultForExecutionWhenItIsSemiAsync [

	| preparedResult |
	preparedResult := query semiAsync prepareNewResult.
	
	self assert: preparedResult class equals: ClySemiAsyncQueryResult.
	self assert: preparedResult buildingQuery == query actualQuery.
	self assert: preparedResult environment == environment.
	self assert: preparedResult isProtected
]
