Class {
	#name : #ClyQueryTest,
	#superclass : #ClyAbsractNavigationEnvironmentTest,
	#instVars : [
		'query',
		'result',
		'resultItems'
	],
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyQueryTest class >> isAbstract [
	^self == ClyQueryTest
]

{ #category : #running }
ClyQueryTest >> createQuery [
	self subclassResponsibility 
]

{ #category : #running }
ClyQueryTest >> executeQuery [

	result := query execute.
	resultItems := result items
]

{ #category : #running }
ClyQueryTest >> setUp [
	super setUp.
	
	query := self createQuery
]

{ #category : #tests }
ClyQueryTest >> testBuildsDefaultResultByDefault [

	self assert: query buildsDefaultResult
]

{ #category : #tests }
ClyQueryTest >> testComparisonOfSameQueries [

	| query2 |
	self assert: query equals: query.
	
	query2 := self createQuery.		
	self assert: query equals: query2.
	self assert: query hash equals: query2 hash
]

{ #category : #tests }
ClyQueryTest >> testComparisonOfSameQueriesWithDifferentRequestedContent [
	| query2 |
	query requiredResult: #contentClass1.
	query2 := self createQuery.
	query2 requiredResult: #contentClass2.

	self deny: query equals: query2
]

{ #category : #tests }
ClyQueryTest >> testConvertingToAsyncQuery [
	| convertedQuery |
	convertedQuery := query async.

	self assert: convertedQuery class equals: ClyAsyncQuery.
	self assert: convertedQuery actualQuery identicalTo: query.
	self assert: convertedQuery requiredResult identicalTo: query requiredResult.
	self assert: convertedQuery scope identicalTo: query scope.
	self assert: convertedQuery asyncResult class equals: ClyAsyncQueryResult
]

{ #category : #tests }
ClyQueryTest >> testConvertingToSemiAsyncQuery [
	| convertedQuery |
	convertedQuery := query semiAsync.

	self assert: convertedQuery class equals: ClyAsyncQuery.
	self assert: convertedQuery actualQuery identicalTo: query.
	self assert: convertedQuery requiredResult identicalTo: query requiredResult.
	self assert: convertedQuery scope identicalTo: query scope.
	self assert: convertedQuery asyncResult class equals: ClySemiAsyncQueryResult
]

{ #category : #tests }
ClyQueryTest >> testConvertingWithNewRequiredResult [
	| newResult convertedQuery |
	newResult := ClyRawQueryResult new.
	convertedQuery := query withResult: newResult.

	self assert: convertedQuery class equals: query class.
	self assert: convertedQuery scope equals: query scope.
	self assert: convertedQuery requiredResult identicalTo: newResult
]

{ #category : #tests }
ClyQueryTest >> testFixingStateBeforeExecution [

	query fixStateBeforeExecution.
	
	self assert: query isReadOnlyObject.
	self assert: query scope isReadOnlyObject
]

{ #category : #tests }
ClyQueryTest >> testGettingEnvironment [

	self assert: query environment equals: environment
]

{ #category : #tests }
ClyQueryTest >> testHasReadonlyRequiredResultByDefault [

	self assert: query requiredResult isReadOnlyObject
]

{ #category : #tests }
ClyQueryTest >> testHasRequiredResultByDefault [

	self assert: query requiredResult notNil
]

{ #category : #tests }
ClyQueryTest >> testHasUnknownScopeByDefault [

	query := query class new.
	self assert: query scope equals: ClyUnknownScope instance
]

{ #category : #tests }
ClyQueryTest >> testIsBoundToEnvironmentWhenScopeIs [

	self assert: query scope isBoundToEnvironment.
	self assert: query isBoundToEnvironment 
]

{ #category : #tests }
ClyQueryTest >> testNotBoundToEnvironmentByDefault [

	query := query class new.
	self deny: query isBoundToEnvironment 
]

{ #category : #tests }
ClyQueryTest >> testPrepareResultForExecution [
	| preparedResult |
	preparedResult := query prepareNewResult.

	self assert: preparedResult class equals: query requiredResult class.
	self assert: preparedResult buildingQuery identicalTo: query.
	self assert: preparedResult environment identicalTo: environment
]
