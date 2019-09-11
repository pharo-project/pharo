Class {
	#name : #ClyQueryTestCase,
	#superclass : #ClyNavigationEnvironmentTestCase,
	#instVars : [
		'query',
		'result',
		'resultItems'
	],
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyQueryTestCase class >> isAbstract [
	^self == ClyQueryTestCase
]

{ #category : #running }
ClyQueryTestCase >> createQuery [
	self subclassResponsibility 
]

{ #category : #running }
ClyQueryTestCase >> executeQuery [

	result := query execute.
	resultItems := result items
]

{ #category : #running }
ClyQueryTestCase >> setUp [
	super setUp.
	
	query := self createQuery
]

{ #category : #tests }
ClyQueryTestCase >> testBuildsDefaultResultByDefault [

	self assert: query buildsDefaultResult
]

{ #category : #tests }
ClyQueryTestCase >> testComparisonOfSameQueries [

	| query2 |
	self assert: query equals: query.
	
	query2 := self createQuery.		
	self assert: query equals: query2.
	self assert: query hash equals: query2 hash
]

{ #category : #tests }
ClyQueryTestCase >> testComparisonOfSameQueriesWithDifferentRequestedContent [

	| query2 |
	query requiredResult: #contentClass1.
	query2 := self createQuery.
	query2 requiredResult: #contentClass2.
		
	self deny: query = query2
]

{ #category : #tests }
ClyQueryTestCase >> testConvertingToAsyncQuery [

	| convertedQuery |
	convertedQuery := query async.
	
	self assert: convertedQuery class equals: ClyAsyncQuery.
	self assert: convertedQuery actualQuery == query.
	self assert: convertedQuery requiredResult == query requiredResult.
	self assert: convertedQuery scope == query scope.
	self assert: convertedQuery asyncResult class equals: ClyAsyncQueryResult
]

{ #category : #tests }
ClyQueryTestCase >> testConvertingToSemiAsyncQuery [

	| convertedQuery |
	convertedQuery := query semiAsync.
	
	self assert: convertedQuery class equals: ClyAsyncQuery.
	self assert: convertedQuery actualQuery == query.
	self assert: convertedQuery requiredResult == query requiredResult.
	self assert: convertedQuery scope == query scope.
	self assert: convertedQuery asyncResult class equals: ClySemiAsyncQueryResult
]

{ #category : #tests }
ClyQueryTestCase >> testConvertingWithNewRequiredResult [

	| newResult convertedQuery |
	newResult := ClyRawQueryResult new.
	convertedQuery := query withResult: newResult.
	
	self assert: convertedQuery class equals: query class.
	self assert: convertedQuery scope equals: query scope.
	self assert: convertedQuery requiredResult == newResult
]

{ #category : #tests }
ClyQueryTestCase >> testFixingStateBeforeExecution [

	query fixStateBeforeExecution.
	
	self assert: query isReadOnlyObject.
	self assert: query scope isReadOnlyObject
]

{ #category : #tests }
ClyQueryTestCase >> testGettingEnvironment [

	self assert: query environment equals: environment
]

{ #category : #tests }
ClyQueryTestCase >> testHasReadonlyRequiredResultByDefault [

	self assert: query requiredResult isReadOnlyObject
]

{ #category : #tests }
ClyQueryTestCase >> testHasRequiredResultByDefault [

	self assert: query requiredResult notNil
]

{ #category : #tests }
ClyQueryTestCase >> testHasUnknownScopeByDefault [

	query := query class new.
	self assert: query scope equals: ClyUnknownScope instance
]

{ #category : #tests }
ClyQueryTestCase >> testIsBoundToEnvironmentWhenScopeIs [

	self assert: query scope isBoundToEnvironment.
	self assert: query isBoundToEnvironment 
]

{ #category : #tests }
ClyQueryTestCase >> testNotBoundToEnvironmentByDefault [

	query := query class new.
	self deny: query isBoundToEnvironment 
]

{ #category : #tests }
ClyQueryTestCase >> testPrepareResultForExecution [

	| preparedResult |
	preparedResult := query prepareNewResult.
	
	self assert: preparedResult class equals: query requiredResult class.
	self assert: preparedResult buildingQuery == query.
	self assert: preparedResult environment == environment.
]
