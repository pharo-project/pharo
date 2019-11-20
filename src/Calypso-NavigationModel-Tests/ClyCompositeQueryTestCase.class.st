Class {
	#name : #ClyCompositeQueryTestCase,
	#superclass : #ClyQueryTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyCompositeQueryTestCase class >> isAbstract [
	^self = ClyCompositeQueryTestCase
]

{ #category : #running }
ClyCompositeQueryTestCase >> queryClass [
	self subclassResponsibility 
]

{ #category : #running }
ClyCompositeQueryTestCase >> subqueryFromScope: aScope [
	aScope bindTo: environment.
	^ ClyQueryExampleReturningScopeBasis
		from: aScope
]

{ #category : #running }
ClyCompositeQueryTestCase >> subqueryFromScope: aScopeClass of: aBasisObject [
	^self subqueryFromScope: (aScopeClass of: aBasisObject)
]

{ #category : #running }
ClyCompositeQueryTestCase >> subqueryFromScopeOf: aBasisObject [
	^self subqueryFromScope: ClyScopeExample of: aBasisObject
]

{ #category : #tests }
ClyCompositeQueryTestCase >> testCompareWithSimilarQueryWhichSubqueryHasDifferentScope [
	| query2 |
	query2 := self queryClass with: {(self subqueryFromScopeOf: self class superclass)}.

	self deny: query equals: query2
]

{ #category : #tests }
ClyCompositeQueryTestCase >> testConvertingAsRestrictedByAnotherScope [
	| convertedQuery restrictedScope |
	restrictedScope := ClyScopeExample2 of: #restrictedBasis.

	convertedQuery := query restrictedByScope: restrictedScope.

	self assert: convertedQuery class equals: query class.
	self assert: convertedQuery requiredResult identicalTo: query requiredResult.
	self assert: (convertedQuery subqueries collect: #class) equals: (query subqueries collect: #class).
	self assert: (convertedQuery subqueries collect: #scope) equals: (query subqueries collect: [ :each | each scope withNewBasisObjects: #(restrictedBasis) ])
]

{ #category : #tests }
ClyCompositeQueryTestCase >> testConvertingWithNewScope [
	| newScope convertedQuery |
	newScope := ClyScopeExample2 of: #someBasis in: environment.

	convertedQuery := query withScope: newScope.

	self assert: convertedQuery class equals: query class.
	self assert: convertedQuery requiredResult identicalTo: query requiredResult.
	self assert: (convertedQuery subqueries collect: #class) equals: (query subqueries collect: #class).
	self assert: (convertedQuery subqueries collect: #scope as: Set) equals: {newScope} asSet
]

{ #category : #tests }
ClyCompositeQueryTestCase >> testConvertingWithNewScopeBasis [
	| convertedQuery |
	convertedQuery := query withScopeOf: #(newBasis).

	self assert: convertedQuery class equals: query class.
	self assert: convertedQuery requiredResult identicalTo: query requiredResult.
	self assert: (convertedQuery subqueries collect: #class) equals: (query subqueries collect: #class).
	self assert: (convertedQuery subqueries collect: #scope) equals: (query subqueries collect: [ :each | each scope withNewBasisObjects: #(newBasis) ])
]

{ #category : #tests }
ClyCompositeQueryTestCase >> testCreationWithEmptySubqueriesShouldReturnUnknownQuery [
	query := self queryClass with: {}.

	self assert: query identicalTo: ClyUnknownQuery instance
]

{ #category : #tests }
ClyCompositeQueryTestCase >> testExecutesConcreteQueryClass [
	query subqueries: { self subqueryFromScopeOf: #basis }.
	
	self deny: (query executesQuery: query class).
	
	self assert: (query executesQuery: query subqueries anyOne class).
	
	self deny: (query executesQuery: ClyQueryExampleReturningScopeBasis2)
]

{ #category : #tests }
ClyCompositeQueryTestCase >> testFixingStateBeforeExecutionShouldFixSubqueriesState [

	query fixStateBeforeExecution.
	
	query subqueries do: [ :each | 
		self assert: each isReadOnlyObject.
		self assert: each scope isReadOnlyObject]
]

{ #category : #tests }
ClyCompositeQueryTestCase >> testHasCompositeScopeFromSubqueries [

	| scope |
	scope := query scope.
	
	self assert: scope class equals: ClyCompositeScope.
	self assert: scope subscopes equals: (query subqueries collect: #scope).
]

{ #category : #tests }
ClyCompositeQueryTestCase >> testIsExecutedFromConcreteScopeClass [

	self assert: (query isExecutedFromScope: ClyScopeExample).
	self assert: (query isExecutedFromScope: ClyScope).
	
	self deny: (query isExecutedFromScope: Array).
]

{ #category : #tests }
ClyCompositeQueryTestCase >> testIsExecutedFromEmptyScope [

	self deny: query isExecutedFromEmptyScope.
	
	query := self queryClass with: { self subqueryFromScope: ClyScopeExample empty}.
	self assert: query isExecutedFromEmptyScope
]

{ #category : #tests }
ClyCompositeQueryTestCase >> testIsExecutedFromMultipleScopeWhenSingleSubqueryIsBasedOnIt [

	query subqueries: { 
		self subqueryFromScope: (ClyScopeExample ofAll: { self class. self class superclass}).
	}.
	
	self assert: query isExecutedFromMultipleScope
]

{ #category : #tests }
ClyCompositeQueryTestCase >> testIsExecutedFromMultipleScopeWhenSubqueriesAreFromScopesOfDifferentObjects [

	query subqueries: { 
		self subqueryFromScopeOf: self class.
		self subqueryFromScopeOf: self class superclass.
	}.
	
	self assert: query isExecutedFromMultipleScope.
	
]

{ #category : #tests }
ClyCompositeQueryTestCase >> testIsExecutedFromScopeOfBasisObject [
	
	self assert: (query isExecutedFromScopeOf: query scope basisObjects anyOne).
	
	self deny: (query isExecutedFromScopeOf: #someObject).
]

{ #category : #tests }
ClyCompositeQueryTestCase >> testIsExecutedFromSingleScopeWhenSingleSubqueryIsBasedOnIt [

	query subqueries: { 
		self subqueryFromScopeOf: self class.
	}.
	
	self assert: query isExecutedFromSingleScope
]

{ #category : #tests }
ClyCompositeQueryTestCase >> testIsExecutedFromSingleScopeWhenSubqueriesAreFromDifferentScopesOfSameObject [

	query subqueries: { 
		self subqueryFromScope: ClyScopeExample of: self class.
		self subqueryFromScope: ClyScopeExample2 of: self class.
	}.
	
	self assert: query isExecutedFromSingleScope
]

{ #category : #tests }
ClyCompositeQueryTestCase >> testNotExecutedFromMultipleScopeWhenAllSubqueriesAreFromEmptyScope [

	query subqueries: { 
		self subqueryFromScope: ClyScopeExample empty.
		self subqueryFromScope: ClyScopeExample2 empty.
	}.
	
	self deny: query isExecutedFromMultipleScope
]

{ #category : #tests }
ClyCompositeQueryTestCase >> testNotExecutedFromMultipleScopeWhenSingleSubqueryIsBasedOnScopeOfSingleObject [

	query subqueries: { 
		self subqueryFromScopeOf: self class.
	}.
	
	self deny: query isExecutedFromMultipleScope
]

{ #category : #tests }
ClyCompositeQueryTestCase >> testNotExecutedFromMultipleScopeWhenSingleSubqueryIsFromEmptyScope [

	query subqueries: { 
		self subqueryFromScope: ClyScopeExample empty.
	}.
	
	self deny: query isExecutedFromMultipleScope
]

{ #category : #tests }
ClyCompositeQueryTestCase >> testNotExecutedFromMultipleScopeWhenSubqueriesAreFromDifferentScopesOfSameObject [

	query subqueries: { 
		self subqueryFromScope: ClyScopeExample of: self class.
		self subqueryFromScope: ClyScopeExample2 of: self class.
	}.
	
	self deny: query isExecutedFromMultipleScope
]

{ #category : #tests }
ClyCompositeQueryTestCase >> testNotExecutedFromSingleScopeWhenAllSubqueriesAreFromEmptyScope [

	query subqueries: { 
		self subqueryFromScope: ClyScopeExample empty.
		self subqueryFromScope: ClyScopeExample2 empty.
	}.
	
	self deny: query isExecutedFromSingleScope
]

{ #category : #tests }
ClyCompositeQueryTestCase >> testNotExecutedFromSingleScopeWhenSubqueriesAreFromScopesOfDifferentObjects [

	query subqueries: { 
		self subqueryFromScopeOf: self class.
		self subqueryFromScopeOf: self class superclass.
	}.
	
	self deny: query isExecutedFromSingleScope.
	
]
