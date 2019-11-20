Class {
	#name : #ClyWrapQueryTestCase,
	#superclass : #ClyCompositeQueryTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyWrapQueryTestCase class >> isAbstract [
	^self = ClyWrapQueryTestCase 
]

{ #category : #tests }
ClyWrapQueryTestCase >> testConvertingWithNewRequiredResultShouldConvertActualQueryToo [
	| newResult convertedQuery |
	newResult := ClyRawQueryResult new.
	convertedQuery := query withResult: newResult.

	self assert: convertedQuery actualQuery requiredResult identicalTo: newResult
]

{ #category : #tests }
ClyWrapQueryTestCase >> testHasCompositeScopeFromSubqueries [
	<expectedFailure>
	"Async query must be created on single subquery.
	And in that case scope will be same as scope of this subquery"
	super testHasCompositeScopeFromSubqueries
]

{ #category : #tests }
ClyWrapQueryTestCase >> testHasScopeFromSubquery [

	self assert: query scope equals: query subqueries anyOne scope
]

{ #category : #tests }
ClyWrapQueryTestCase >> testIsExecutedFromMultipleScopeWhenSubqueriesAreFromScopesOfDifferentObjects [
	<expectedFailure>
	"Async query forbid creation on several subqueries.
	It shoud always wrap single query"
	super testIsExecutedFromMultipleScopeWhenSubqueriesAreFromScopesOfDifferentObjects
]

{ #category : #tests }
ClyWrapQueryTestCase >> testIsExecutedFromSingleScopeWhenSubqueriesAreFromDifferentScopesOfSameObject [
	<expectedFailure>
	"Async query forbid creation on several subqueries.
	It shoud always wrap single query"
	super testIsExecutedFromSingleScopeWhenSubqueriesAreFromDifferentScopesOfSameObject
]

{ #category : #tests }
ClyWrapQueryTestCase >> testNotExecutedFromMultipleScopeWhenAllSubqueriesAreFromEmptyScope [
	<expectedFailure>
	"Async query forbid creation on several subqueries.
	It shoud always wrap single query"
	super testNotExecutedFromMultipleScopeWhenAllSubqueriesAreFromEmptyScope
]

{ #category : #tests }
ClyWrapQueryTestCase >> testNotExecutedFromMultipleScopeWhenSubqueriesAreFromDifferentScopesOfSameObject [
	<expectedFailure>
	"Async query forbid creation on several subqueries.
	It shoud always wrap single query"
	super testNotExecutedFromMultipleScopeWhenSubqueriesAreFromDifferentScopesOfSameObject
]

{ #category : #tests }
ClyWrapQueryTestCase >> testNotExecutedFromSingleScopeWhenAllSubqueriesAreFromEmptyScope [
	<expectedFailure>
	"Async query forbid creation on several subqueries.
	It shoud always wrap single query"
	super testNotExecutedFromSingleScopeWhenAllSubqueriesAreFromEmptyScope
]

{ #category : #tests }
ClyWrapQueryTestCase >> testNotExecutedFromSingleScopeWhenSubqueriesAreFromScopesOfDifferentObjects [
	<expectedFailure>
	"Async query forbid creation on several subqueries.
	It shoud always wrap single query"
	super testNotExecutedFromSingleScopeWhenSubqueriesAreFromScopesOfDifferentObjects
]
