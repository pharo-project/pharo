Class {
	#name : #ClyNavigationEnvironmentTests,
	#superclass : #ClyNavigationEnvironmentTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #helpers }
ClyNavigationEnvironmentTests >> createQueryFromScope: aScope [
	aScope bindTo: environment.
	^ ClyQueryExampleReturningScopeBasis from: aScope
]

{ #category : #helpers }
ClyNavigationEnvironmentTests >> createQueryFromScopeOf: aBasisObject [
	^ self createQueryFromScope: (ClyScopeExample of: aBasisObject)
]

{ #category : #tests }
ClyNavigationEnvironmentTests >> testCheckEmptyQueryWhenThereIsEmptyResult [

	| query result |
	query := self createQueryFromScope: ClyScopeExample empty.
	result := environment query: query.
	self assert: result items isEmpty.
	
	self assert: (environment isQueryEmpty: query)
]

{ #category : #tests }
ClyNavigationEnvironmentTests >> testCheckEmptyQueryWhenThereIsNoResultButItWouldBeEmpty [

	| query |
	query := self createQueryFromScope: ClyScopeExample empty.
	
	self assert: (environment isQueryEmpty: query)
]

{ #category : #tests }
ClyNavigationEnvironmentTests >> testCheckEmptyQueryWhenThereIsNoResultButItWouldNotBeEmpty [

	| query |
	query := self createQueryFromScopeOf: self class.
	
	self deny: (environment isQueryEmpty: query)
]

{ #category : #tests }
ClyNavigationEnvironmentTests >> testCheckEmptyQueryWhenThereIsNotEmptyResult [

	| query result |
	query := self createQueryFromScopeOf: self class.
	result := environment query: query.
	self assert: result items notEmpty.
	
	self deny: (environment isQueryEmpty: query)
]

{ #category : #tests }
ClyNavigationEnvironmentTests >> testMakeQueryOfResultAsReadOnlyObjectWhenItIsExecuted [

	| query |
	query := self createQueryFromScopeOf: self class.
	environment query: query.
	
	self assert: query isReadOnlyObject
]

{ #category : #tests }
ClyNavigationEnvironmentTests >> testMakeQueryScopeOfResultAsReadOnlyObjectWhenItIsExecuted [

	| query |
	query := self createQueryFromScopeOf: self class.
	environment query: query.
	
	self assert: query scope isReadOnlyObject
]

{ #category : #tests }
ClyNavigationEnvironmentTests >> testProcessingQueryCacheWhenSomeWasCollectedAsGarbage [

	| evaluated |
	environment query: (self createQueryFromScopeOf: self class).
	Smalltalk garbageCollectMost; garbageCollectMost.
	
	evaluated := false.
	environment withCachedQueriesDo: [ :each | evaluated := true ].
	
	self deny: evaluated
]

{ #category : #tests }
ClyNavigationEnvironmentTests >> testQueryCacheShouldBeWeak [

	| cache query |
	query := self createQueryFromScopeOf: self class.
	environment query: query.	
	Smalltalk garbageCollectMost; garbageCollectMost.
	
	cache := environment cachedResultOf: query.
	self assert: cache equals: nil
]

{ #category : #tests }
ClyNavigationEnvironmentTests >> testQueryExecutionShouldRemoveOldWeaklyGarbagedCache [

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
ClyNavigationEnvironmentTests >> testQueryExecutionWhenResultWasWeaklyCollectedAsGarbage [

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
ClyNavigationEnvironmentTests >> testQueryResultShouldBeCached [

	| result1 result2 |
	result1 := environment query: (self createQueryFromScopeOf: self class).
	result2 := environment query: (self createQueryFromScopeOf: self class).	
		
	self assert: result1 == result2
]
