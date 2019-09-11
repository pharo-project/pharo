Class {
	#name : #ClyUnionQueryTest,
	#superclass : #ClyCompositeQueryTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #running }
ClyUnionQueryTest >> createQuery [
	^ClyUnionQuery with: { 
		ClyQueryExampleReturningScopeBasis from: ClyScopeExample of: self class in: environment.
		ClyQueryExampleReturningScopeBasis2 from: ClyScopeExample2 of: self class superclass in: environment}
]

{ #category : #running }
ClyUnionQueryTest >> queryClass [
	^ClyUnionQuery
]

{ #category : #tests }
ClyUnionQueryTest >> testConvertingWithNewScopeWhenAllSubqueriesAreSameKind [
	| newScope convertedQuery |
	query
		subqueries:
			{(self subqueryFromScope: ClyScopeExample of: Array).
			(self subqueryFromScope: ClyScopeExample2 of: String)}.
	newScope := ClyScopeExample2 of: #someBasis in: environment.
	convertedQuery := query withScope: newScope.
	self
		assert: convertedQuery class
		equals: query subqueries anyOne class.
	self assert: convertedQuery scope equals: newScope.
	self assert: convertedQuery requiredResult == query requiredResult
]

{ #category : #tests }
ClyUnionQueryTest >> testCreationFromAnotherCompositeQueryAndNewDifferentSubquery [

	| subquery1 subquery2 subquery3 |
	subquery1 := self subqueryFromScope: ClyScopeExample of: Array.
	subquery2 := self subqueryFromScope: ClyScopeExample2 of: String.
	subquery3 := ClyQueryExampleReturningScopeBasis2 from: ClyScopeExample of: Object in: environment.
	
	query := subquery1, subquery2, subquery3.
	
	self assert: query class equals: ClyUnionQuery.
	self assert: query subqueries equals: {subquery1. subquery2. subquery3} asSet.
	self assert: query requiredResult equals: subquery1 requiredResult
]

{ #category : #tests }
ClyUnionQueryTest >> testCreationFromAnotherCompositeQueryAndSimilarQuery [

	| subquery1 subquery2 subquery3 |
	subquery1 := self subqueryFromScopeOf: Array.
	subquery2 := self subqueryFromScope: ClyScopeExample2 of: String.
	subquery3 := self subqueryFromScopeOf: Object.	
		
	query := subquery1, subquery2, subquery3.
	
	self assert: query class equals: self queryClass.
	self assert: query subqueries equals: { 
		self subqueryFromScope: (ClyScopeExample ofAll: { Array. Object }). subquery2} asSet.
	self assert: query requiredResult equals: subquery1 requiredResult
]

{ #category : #tests }
ClyUnionQueryTest >> testCreationFromTwoDifferentTypedQueries [

	| subquery1 subquery2 |
	subquery1 := ClyQueryExampleReturningScopeBasis from: ClyScopeExample of: Array in: environment.
	subquery2 := ClyQueryExampleReturningScopeBasis2 from: ClyScopeExample of: String in: environment.
	
	query := subquery1, subquery2.
	
	self assert: query class equals: self queryClass.
	self assert: query subqueries equals: {subquery1. subquery2} asSet.
	self assert: query requiredResult == subquery1 requiredResult
]

{ #category : #tests }
ClyUnionQueryTest >> testCreationFromTwoQueriesOfSameTypeAndScopeButWithDifferentParameter [

	| subquery1 subquery2 |
	subquery1 := self subqueryFromScopeOf: Array.
	subquery1 extraParameter: #testParameter.
	subquery2 := self subqueryFromScopeOf: Array.
	subquery2 extraParameter: #testParameter2.
	query := subquery1, subquery2.
	
	self assert: query class equals: self queryClass.
	self assert: query subqueries equals: {subquery1. subquery2} asSet.
	self assert: query requiredResult == subquery1 requiredResult
]

{ #category : #tests }
ClyUnionQueryTest >> testCreationFromTwoQueriesOfSameTypeButDifferentScope [

	| subquery1 subquery2 |
	subquery1 := self subqueryFromScope: ClyScopeExample of: Array.
	subquery2 := self subqueryFromScope: ClyScopeExample2 of: String.
	
	query := subquery1, subquery2.
	
	self assert: query class equals: self queryClass.
	self assert: query subqueries asSet equals: {subquery1. subquery2} asSet.
	self assert: query requiredResult == subquery1 requiredResult
]

{ #category : #tests }
ClyUnionQueryTest >> testCreationFromTwoQueriesOfSameTypeSimilarScope [
	| subquery1 subquery2 |
	subquery1 := self subqueryFromScope: ClyScopeExample of: Array.
	subquery2 := self subqueryFromScope: (ClyScopeExample ofAll: {String. Array}).
	
	query := subquery1 , subquery2.
	
	self assert: query class equals: subquery1 class.
	self deny: query == subquery1.
	self 
		assert: query scope 
		equals: (ClyScopeExample ofAll: {Array. String} in: environment).
	self assert: query requiredResult == subquery1 requiredResult
]

{ #category : #tests }
ClyUnionQueryTest >> testCreationFromTwoSameQueriesOfSameScopeButWithDifferentParameter [

	| subquery1 subquery2 |
	subquery1 := self subqueryFromScopeOf: Array.
	subquery1 scope extraParameter: #testParameter.
	subquery2 := self subqueryFromScopeOf: Array.
	subquery2 scope extraParameter: #testParameter2.
	query := subquery1, subquery2.
	
	self assert: query class equals: self queryClass.
	self assert: query subqueries equals: {subquery1. subquery2} asSet.
	self assert: query requiredResult == subquery1 requiredResult
]

{ #category : #tests }
ClyUnionQueryTest >> testCreationFromTwoUnionQueries [
	| subquery1 subquery2 subquery3 subquery4 unionQuery1 unionQuery2 scopes |
	subquery1 := self subqueryFromScope: ClyScopeExample of: Array.
	subquery2 := self subqueryFromScope: ClyScopeExample2 of: String.
	subquery3 := self subqueryFromScope: ClyScopeExample of: Object.
	subquery4 := self subqueryFromScope: ClyScopeExample2 of: Class.
	
	unionQuery1 := subquery1 , subquery2.
	unionQuery2 := subquery3 , subquery4.
	query := unionQuery1 , unionQuery2.
	
	self assert: query class equals: self queryClass.
	self assert: query subqueries size equals: 2.
	self
		assert: (query subqueries collect: #class as: Set)
		equals: {subquery1 class} asSet.
	scopes := query subqueries collect: #scope as: Set.
	self assert: scopes equals: {
		ClyScopeExample ofAll: {Array. Object}.
		ClyScopeExample2 ofAll: {String. Class}} asSet.
	self assert: query requiredResult == subquery1 requiredResult
]

{ #category : #tests }
ClyUnionQueryTest >> testCreationWithSingleSubquery [

	| subquery |
	subquery := self subqueryFromScopeOf: Array.

	query := ClyUnionQuery with: {subquery}.
	
	self assert: query equals: subquery.
	self deny: query == subquery
]

{ #category : #tests }
ClyUnionQueryTest >> testCreationWithSingleSubqueryButAnotherResult [

	| subquery |
	subquery := self subqueryFromScopeOf: Array.

	query := ClyUnionQuery with: {subquery} as: #specialResult.
	
	self assert: query class equals: subquery class.
	self assert: query scope equals: subquery scope.
	self assert: query requiredResult equals: #specialResult.
	self deny: subquery requiredResult = #specialResult.
]

{ #category : #tests }
ClyUnionQueryTest >> testEmptySubqueriesAreForbidden [

	[query subqueries: {}.
	self assert: false description: 'empty subqueries should be forbidden'] ifError: [].
]

{ #category : #tests }
ClyUnionQueryTest >> testExecution [

	query subqueries: { 
		self subqueryFromScope: ClyScopeExample of: self class.
		self subqueryFromScope: ClyScopeExample2 of: self class superclass.
	}.
	self executeQuery.
	
	self assert: resultItems size equals: 2.
	self assert: resultItems asSet equals: {self class. self class superclass} asSet
]

{ #category : #tests }
ClyUnionQueryTest >> testExecutionShouldRemoveDuplication [

	query subqueries: { 
		self subqueryFromScope: ClyScopeExample of: self class.
		self subqueryFromScope: ClyScopeExample2 of: self class.
	}.
	self executeQuery.
	
	self assert: resultItems asArray equals: {self class }
]

{ #category : #tests }
ClyUnionQueryTest >> testIsEmptyWhenAllSubqueriesAreEmpty [
	query subqueries: {
		(self subqueryFromScope: ClyScopeExample empty).
		(self subqueryFromScope: ClyScopeExample2 empty)
	}.
	
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyUnionQueryTest >> testNotEmptyWhenThereIsNotEmptySubquery [
	query subqueries: {
		(self subqueryFromScope: ClyScopeExample empty).
		(self subqueryFromScope: ClyScopeExample2 of: self class superclass)
	}.
	
	self deny: query checkEmptyResult
]
