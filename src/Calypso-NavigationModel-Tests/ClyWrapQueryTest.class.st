Class {
	#name : #ClyWrapQueryTest,
	#superclass : #ClyCompositeQueryTest,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyWrapQueryTest class >> isAbstract [
	^self = ClyWrapQueryTest 
]

{ #category : #tests }
ClyWrapQueryTest >> testConvertingWithNewRequiredResultShouldConvertActualQueryToo [
	| newResult convertedQuery |
	newResult := ClyRawQueryResult new.
	convertedQuery := query withResult: newResult.

	self assert: convertedQuery actualQuery requiredResult identicalTo: newResult
]

{ #category : #tests }
ClyWrapQueryTest >> testHasCompositeScopeFromSubqueries [
	<expectedFailure>
	"Async query must be created on single subquery.
	And in that case scope will be same as scope of this subquery"
	super testHasCompositeScopeFromSubqueries
]

{ #category : #tests }
ClyWrapQueryTest >> testHasScopeFromSubquery [

	self assert: query scope equals: query subqueries anyOne scope
]

{ #category : #tests }
ClyWrapQueryTest >> testIsExecutedFromMultipleScopeWhenSubqueriesAreFromScopesOfDifferentObjects [
	<expectedFailure>
	"Async query forbid creation on several subqueries.
	It shoud always wrap single query"
	super testIsExecutedFromMultipleScopeWhenSubqueriesAreFromScopesOfDifferentObjects
]

{ #category : #tests }
ClyWrapQueryTest >> testIsExecutedFromSingleScopeWhenSubqueriesAreFromDifferentScopesOfSameObject [
	<expectedFailure>
	"Async query forbid creation on several subqueries.
	It shoud always wrap single query"
	super testIsExecutedFromSingleScopeWhenSubqueriesAreFromDifferentScopesOfSameObject
]

{ #category : #tests }
ClyWrapQueryTest >> testNotExecutedFromMultipleScopeWhenAllSubqueriesAreFromEmptyScope [
	<expectedFailure>
	"Async query forbid creation on several subqueries.
	It shoud always wrap single query"
	super testNotExecutedFromMultipleScopeWhenAllSubqueriesAreFromEmptyScope
]

{ #category : #tests }
ClyWrapQueryTest >> testNotExecutedFromMultipleScopeWhenSubqueriesAreFromDifferentScopesOfSameObject [
	<expectedFailure>
	"Async query forbid creation on several subqueries.
	It shoud always wrap single query"
	super testNotExecutedFromMultipleScopeWhenSubqueriesAreFromDifferentScopesOfSameObject
]

{ #category : #tests }
ClyWrapQueryTest >> testNotExecutedFromSingleScopeWhenAllSubqueriesAreFromEmptyScope [
	<expectedFailure>
	"Async query forbid creation on several subqueries.
	It shoud always wrap single query"
	super testNotExecutedFromSingleScopeWhenAllSubqueriesAreFromEmptyScope
]

{ #category : #tests }
ClyWrapQueryTest >> testNotExecutedFromSingleScopeWhenSubqueriesAreFromScopesOfDifferentObjects [
	<expectedFailure>
	"Async query forbid creation on several subqueries.
	It shoud always wrap single query"
	super testNotExecutedFromSingleScopeWhenSubqueriesAreFromScopesOfDifferentObjects
]
