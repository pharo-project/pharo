Class {
	#name : #ClyUnionQueryTest,
	#superclass : #ClyCompositeQueryTest,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #running }
ClyUnionQueryTest >> createQuery [
	^ClyUnionQuery with: { 
		ClyReturningScopeBasisExampleQuery from: ClyExampleScope of: self class in: environment.
		ClyReturningScopeBasis2ExampleQuery from: ClyExample2Scope of: self class superclass in: environment}
]

{ #category : #running }
ClyUnionQueryTest >> queryClass [
	^ClyUnionQuery
]

{ #category : #tests }
ClyUnionQueryTest >> testConvertingWithNewScopeWhenAllSubqueriesAreSameKind [
	| newScope convertedQuery |
	query subqueries: {(self subqueryFromScope: ClyExampleScope of: Array) . (self subqueryFromScope: ClyExample2Scope of: String)}.
	newScope := ClyExample2Scope of: #someBasis in: environment.
	convertedQuery := query withScope: newScope.
	self assert: convertedQuery class equals: query subqueries anyOne class.
	self assert: convertedQuery scope equals: newScope.
	self assert: convertedQuery requiredResult identicalTo: query requiredResult
]

{ #category : #tests }
ClyUnionQueryTest >> testCreationFromAnotherCompositeQueryAndNewDifferentSubquery [

	| subquery1 subquery2 subquery3 |
	subquery1 := self subqueryFromScope: ClyExampleScope of: Array.
	subquery2 := self subqueryFromScope: ClyExample2Scope of: String.
	subquery3 := ClyReturningScopeBasis2ExampleQuery from: ClyExampleScope of: Object in: environment.
	
	query := subquery1, subquery2, subquery3.
	
	self assert: query class equals: ClyUnionQuery.
	self assert: query subqueries equals: {subquery1. subquery2. subquery3} asSet.
	self assert: query requiredResult equals: subquery1 requiredResult
]

{ #category : #tests }
ClyUnionQueryTest >> testCreationFromAnotherCompositeQueryAndSimilarQuery [

	| subquery1 subquery2 subquery3 |
	subquery1 := self subqueryFromScopeOf: Array.
	subquery2 := self subqueryFromScope: ClyExample2Scope of: String.
	subquery3 := self subqueryFromScopeOf: Object.	
		
	query := subquery1, subquery2, subquery3.
	
	self assert: query class equals: self queryClass.
	self assert: query subqueries equals: { 
		self subqueryFromScope: (ClyExampleScope ofAll: { Array. Object }). subquery2} asSet.
	self assert: query requiredResult equals: subquery1 requiredResult
]

{ #category : #tests }
ClyUnionQueryTest >> testCreationFromTwoDifferentTypedQueries [
	| subquery1 subquery2 |
	subquery1 := ClyReturningScopeBasisExampleQuery from: ClyExampleScope of: Array in: environment.
	subquery2 := ClyReturningScopeBasis2ExampleQuery from: ClyExampleScope of: String in: environment.

	query := subquery1 , subquery2.

	self assert: query class equals: self queryClass.
	self assert: query subqueries equals: {subquery1 . subquery2} asSet.
	self assert: query requiredResult identicalTo: subquery1 requiredResult
]

{ #category : #tests }
ClyUnionQueryTest >> testCreationFromTwoQueriesOfSameTypeAndScopeButWithDifferentParameter [
	| subquery1 subquery2 |
	subquery1 := self subqueryFromScopeOf: Array.
	subquery1 extraParameter: #testParameter.
	subquery2 := self subqueryFromScopeOf: Array.
	subquery2 extraParameter: #testParameter2.
	query := subquery1 , subquery2.

	self assert: query class equals: self queryClass.
	self assert: query subqueries equals: {subquery1 . subquery2} asSet.
	self assert: query requiredResult identicalTo: subquery1 requiredResult
]

{ #category : #tests }
ClyUnionQueryTest >> testCreationFromTwoQueriesOfSameTypeButDifferentScope [
	| subquery1 subquery2 |
	subquery1 := self subqueryFromScope: ClyExampleScope of: Array.
	subquery2 := self subqueryFromScope: ClyExample2Scope of: String.

	query := subquery1 , subquery2.

	self assert: query class equals: self queryClass.
	self assert: query subqueries asSet equals: {subquery1 . subquery2} asSet.
	self assert: query requiredResult identicalTo: subquery1 requiredResult
]

{ #category : #tests }
ClyUnionQueryTest >> testCreationFromTwoQueriesOfSameTypeSimilarScope [
	| subquery1 subquery2 |
	subquery1 := self subqueryFromScope: ClyExampleScope of: Array.
	subquery2 := self subqueryFromScope: (ClyExampleScope ofAll: {String . Array}).

	query := subquery1 , subquery2.

	self assert: query class equals: subquery1 class.
	self deny: query identicalTo: subquery1.
	self assert: query scope equals: (ClyExampleScope ofAll: {Array . String} in: environment).
	self assert: query requiredResult identicalTo: subquery1 requiredResult
]

{ #category : #tests }
ClyUnionQueryTest >> testCreationFromTwoSameQueriesOfSameScopeButWithDifferentParameter [
	| subquery1 subquery2 |
	subquery1 := self subqueryFromScopeOf: Array.
	subquery1 scope extraParameter: #testParameter.
	subquery2 := self subqueryFromScopeOf: Array.
	subquery2 scope extraParameter: #testParameter2.
	query := subquery1 , subquery2.

	self assert: query class equals: self queryClass.
	self assert: query subqueries equals: {subquery1 . subquery2} asSet.
	self assert: query requiredResult identicalTo: subquery1 requiredResult
]

{ #category : #tests }
ClyUnionQueryTest >> testCreationFromTwoUnionQueries [
	| subquery1 subquery2 subquery3 subquery4 unionQuery1 unionQuery2 scopes |
	subquery1 := self subqueryFromScope: ClyExampleScope of: Array.
	subquery2 := self subqueryFromScope: ClyExample2Scope of: String.
	subquery3 := self subqueryFromScope: ClyExampleScope of: Object.
	subquery4 := self subqueryFromScope: ClyExample2Scope of: Class.

	unionQuery1 := subquery1 , subquery2.
	unionQuery2 := subquery3 , subquery4.
	query := unionQuery1 , unionQuery2.

	self assert: query class equals: self queryClass.
	self assert: query subqueries size equals: 2.
	self assert: (query subqueries collect: #class as: Set) equals: {subquery1 class} asSet.
	scopes := query subqueries collect: #scope as: Set.
	self assert: scopes equals: {(ClyExampleScope ofAll: {Array . Object}) . (ClyExample2Scope ofAll: {String . Class})} asSet.
	self assert: query requiredResult identicalTo: subquery1 requiredResult
]

{ #category : #tests }
ClyUnionQueryTest >> testCreationWithSingleSubquery [
	| subquery |
	subquery := self subqueryFromScopeOf: Array.

	query := ClyUnionQuery with: {subquery}.

	self assert: query equals: subquery.
	self deny: query identicalTo: subquery
]

{ #category : #tests }
ClyUnionQueryTest >> testCreationWithSingleSubqueryButAnotherResult [
	| subquery |
	subquery := self subqueryFromScopeOf: Array.

	query := ClyUnionQuery with: {subquery} as: #specialResult.

	self assert: query class equals: subquery class.
	self assert: query scope equals: subquery scope.
	self assert: query requiredResult equals: #specialResult.
	self deny: subquery requiredResult equals: #specialResult
]

{ #category : #tests }
ClyUnionQueryTest >> testEmptySubqueriesAreForbidden [

	[query subqueries: {}.
	self assert: false description: 'empty subqueries should be forbidden'] onErrorDo: [].
]

{ #category : #tests }
ClyUnionQueryTest >> testExecution [

	query subqueries: { 
		self subqueryFromScope: ClyExampleScope of: self class.
		self subqueryFromScope: ClyExample2Scope of: self class superclass.
	}.
	self executeQuery.
	
	self assert: resultItems size equals: 2.
	self assert: resultItems asSet equals: {self class. self class superclass} asSet
]

{ #category : #tests }
ClyUnionQueryTest >> testExecutionShouldRemoveDuplication [

	query subqueries: { 
		self subqueryFromScope: ClyExampleScope of: self class.
		self subqueryFromScope: ClyExample2Scope of: self class.
	}.
	self executeQuery.
	
	self assert: resultItems asArray equals: {self class }
]

{ #category : #tests }
ClyUnionQueryTest >> testIsEmptyWhenAllSubqueriesAreEmpty [
	query subqueries: {
		(self subqueryFromScope: ClyExampleScope empty).
		(self subqueryFromScope: ClyExample2Scope empty)
	}.
	
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyUnionQueryTest >> testNotEmptyWhenThereIsNotEmptySubquery [
	query subqueries: {
		(self subqueryFromScope: ClyExampleScope empty).
		(self subqueryFromScope: ClyExample2Scope of: self class superclass)
	}.
	
	self deny: query checkEmptyResult
]
