Class {
	#name : #ClyNavigationEnvironmentTest,
	#superclass : #ClyNavigationEnvironmentTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #helpers }
ClyNavigationEnvironmentTest >> createQueryFromScope: aScope [
	aScope bindTo: environment.
	^ ClyQueryExampleReturningScopeBasis from: aScope
]

{ #category : #helpers }
ClyNavigationEnvironmentTest >> createQueryFromScopeOf: aBasisObject [
	^ self createQueryFromScope: (ClyScopeExample of: aBasisObject)
]

{ #category : #tests }
ClyNavigationEnvironmentTest >> testCheckEmptyQueryWhenThereIsEmptyResult [

	| query result |
	query := self createQueryFromScope: ClyScopeExample empty.
	result := environment query: query.
	self assert: result items isEmpty.
	
	self assert: (environment isQueryEmpty: query)
]

{ #category : #tests }
ClyNavigationEnvironmentTest >> testCheckEmptyQueryWhenThereIsNoResultButItWouldBeEmpty [

	| query |
	query := self createQueryFromScope: ClyScopeExample empty.
	
	self assert: (environment isQueryEmpty: query)
]

{ #category : #tests }
ClyNavigationEnvironmentTest >> testCheckEmptyQueryWhenThereIsNoResultButItWouldNotBeEmpty [

	| query |
	query := self createQueryFromScopeOf: self class.
	
	self deny: (environment isQueryEmpty: query)
]

{ #category : #tests }
ClyNavigationEnvironmentTest >> testCheckEmptyQueryWhenThereIsNotEmptyResult [

	| query result |
	query := self createQueryFromScopeOf: self class.
	result := environment query: query.
	self assert: result items notEmpty.
	
	self deny: (environment isQueryEmpty: query)
]

{ #category : #tests }
ClyNavigationEnvironmentTest >> testMakeQueryOfResultAsReadOnlyObjectWhenItIsExecuted [

	| query |
	query := self createQueryFromScopeOf: self class.
	environment query: query.
	
	self assert: query isReadOnlyObject
]

{ #category : #tests }
ClyNavigationEnvironmentTest >> testMakeQueryScopeOfResultAsReadOnlyObjectWhenItIsExecuted [

	| query |
	query := self createQueryFromScopeOf: self class.
	environment query: query.
	
	self assert: query scope isReadOnlyObject
]

{ #category : #tests }
ClyNavigationEnvironmentTest >> testProcessingQueryCacheWhenSomeWasCollectedAsGarbage [

	| evaluated |
	environment query: (self createQueryFromScopeOf: self class).
	Smalltalk garbageCollectMost; garbageCollectMost.
	
	evaluated := false.
	environment withCachedQueriesDo: [ :each | evaluated := true ].
	
	self deny: evaluated
]

{ #category : #tests }
ClyNavigationEnvironmentTest >> testQueryCacheShouldBeWeak [

	| cache query |
	query := self createQueryFromScopeOf: self class.
	environment query: query.	
	Smalltalk garbageCollectMost; garbageCollectMost.
	
	cache := environment cachedResultOf: query.
	self assert: cache equals: nil
]

{ #category : #tests }
ClyNavigationEnvironmentTest >> testQueryExecutionShouldRemoveOldWeaklyGarbagedCache [

	| result query |
	query := self createQueryFromScopeOf: self class.
	environment query: query.
	Smalltalk garbageCollectMost; garbageCollectMost.
	result := environment cachedResultOf: query.
	self assert: result equals: nil.
	
	result := environment query: (self createQueryFromScopeOf: self class superclass).
	self deny: (environment queryCache includesKey: query)
]

{ #category : #tests }
ClyNavigationEnvironmentTest >> testQueryExecutionWhenResultWasWeaklyCollectedAsGarbage [

	| cache result result2 query |
	query := self createQueryFromScopeOf: self class.
	environment query: query.
	Smalltalk garbageCollectMost; garbageCollectMost.
	cache := environment cachedResultOf: query.
	self assert: cache equals: nil.
	
	result := environment query: query.
	self assert: result class equals: ClyRawQueryResult.
	result2 := environment query: query.
	self assert: result == result2
]

{ #category : #tests }
ClyNavigationEnvironmentTest >> testQueryResultShouldBeCached [

	| result1 result2 |
	result1 := environment query: (self createQueryFromScopeOf: self class).
	result2 := environment query: (self createQueryFromScopeOf: self class).	
		
	self assert: result1 == result2
]
