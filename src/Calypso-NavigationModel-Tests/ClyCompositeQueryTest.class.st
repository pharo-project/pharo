Class {
	#name : #ClyCompositeQueryTest,
	#superclass : #ClyQueryTest,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyCompositeQueryTest class >> isAbstract [
	^self = ClyCompositeQueryTest
]

{ #category : #running }
ClyCompositeQueryTest >> queryClass [
	self subclassResponsibility 
]

{ #category : #running }
ClyCompositeQueryTest >> subqueryFromScope: aScope [
	aScope bindTo: environment.
	^ ClyReturningScopeBasisExampleQuery
		from: aScope
]

{ #category : #running }
ClyCompositeQueryTest >> subqueryFromScope: aScopeClass of: aBasisObject [
	^self subqueryFromScope: (aScopeClass of: aBasisObject)
]

{ #category : #running }
ClyCompositeQueryTest >> subqueryFromScopeOf: aBasisObject [
	^self subqueryFromScope: ClyExampleScope of: aBasisObject
]

{ #category : #tests }
ClyCompositeQueryTest >> testCompareWithSimilarQueryWhichSubqueryHasDifferentScope [
	| query2 |
	query2 := self queryClass with: {(self subqueryFromScopeOf: self class superclass)}.

	self deny: query equals: query2
]

{ #category : #tests }
ClyCompositeQueryTest >> testConvertingAsRestrictedByAnotherScope [

	| convertedQuery restrictedScope |
	restrictedScope := ClyExample2Scope of: #restrictedBasis.
	convertedQuery := query restrictedByScope: restrictedScope.
	self assert: convertedQuery class equals: query class.
	self assert: convertedQuery requiredResult identicalTo: query requiredResult.
	self assert: (convertedQuery subqueries collect: [:each | each class]) equals: (query subqueries collect: #class).
	self assert: (convertedQuery subqueries collect: [:each | each scope]) equals: (query subqueries collect: [ :each | each scope withNewBasisObjects: #(restrictedBasis) ])
]

{ #category : #tests }
ClyCompositeQueryTest >> testConvertingWithNewScope [
	| newScope convertedQuery |
	newScope := ClyExample2Scope of: #someBasis in: environment.

	convertedQuery := query withScope: newScope.

	self assert: convertedQuery class equals: query class.
	self assert: convertedQuery requiredResult identicalTo: query requiredResult.
	self assert: (convertedQuery subqueries collect: [:each | each class]) equals: (query subqueries collect: #class).
	self assert: (convertedQuery subqueries collect: [:each | each scope] as: Set) equals: {newScope} asSet
]

{ #category : #tests }
ClyCompositeQueryTest >> testConvertingWithNewScopeBasis [
	| convertedQuery |
	convertedQuery := query withScopeOf: #(newBasis).

	self assert: convertedQuery class equals: query class.
	self assert: convertedQuery requiredResult identicalTo: query requiredResult.
	self assert: (convertedQuery subqueries collect: [:each | each class]) equals: (query subqueries collect: #class).
	self assert: (convertedQuery subqueries collect: [:each | each scope]) equals: (query subqueries collect: [ :each | each scope withNewBasisObjects: #(newBasis) ])
]

{ #category : #tests }
ClyCompositeQueryTest >> testCreationWithEmptySubqueriesShouldReturnUnknownQuery [
	query := self queryClass with: {}.

	self assert: query identicalTo: ClyUnknownQuery instance
]

{ #category : #tests }
ClyCompositeQueryTest >> testExecutesConcreteQueryClass [
	query subqueries: { self subqueryFromScopeOf: #basis }.
	
	self deny: (query executesQuery: query class).
	
	self assert: (query executesQuery: query subqueries anyOne class).
	
	self deny: (query executesQuery: ClyReturningScopeBasis2ExampleQuery)
]

{ #category : #tests }
ClyCompositeQueryTest >> testFixingStateBeforeExecutionShouldFixSubqueriesState [

	query fixStateBeforeExecution.
	
	query subqueries do: [ :each | 
		self assert: each isReadOnlyObject.
		self assert: each scope isReadOnlyObject]
]

{ #category : #tests }
ClyCompositeQueryTest >> testHasCompositeScopeFromSubqueries [

	| scope |
	scope := query scope.
	self assert: scope class equals: ClyCompositeScope.
	self assert: scope subscopes equals: (query subqueries collect: [:each | each scope]).
]

{ #category : #tests }
ClyCompositeQueryTest >> testIsExecutedFromConcreteScopeClass [

	self assert: (query isExecutedFromScope: ClyExampleScope).
	self assert: (query isExecutedFromScope: ClyScope).
	
	self deny: (query isExecutedFromScope: Array).
]

{ #category : #tests }
ClyCompositeQueryTest >> testIsExecutedFromEmptyScope [

	self deny: query isExecutedFromEmptyScope.
	
	query := self queryClass with: { self subqueryFromScope: ClyExampleScope empty}.
	self assert: query isExecutedFromEmptyScope
]

{ #category : #tests }
ClyCompositeQueryTest >> testIsExecutedFromMultipleScopeWhenSingleSubqueryIsBasedOnIt [

	query subqueries: { 
		self subqueryFromScope: (ClyExampleScope ofAll: { self class. self class superclass}).
	}.
	
	self assert: query isExecutedFromMultipleScope
]

{ #category : #tests }
ClyCompositeQueryTest >> testIsExecutedFromMultipleScopeWhenSubqueriesAreFromScopesOfDifferentObjects [

	query subqueries: { 
		self subqueryFromScopeOf: self class.
		self subqueryFromScopeOf: self class superclass.
	}.
	
	self assert: query isExecutedFromMultipleScope.
	
]

{ #category : #tests }
ClyCompositeQueryTest >> testIsExecutedFromScopeOfBasisObject [
	
	self assert: (query isExecutedFromScopeOf: query scope basisObjects anyOne).
	
	self deny: (query isExecutedFromScopeOf: #someObject).
]

{ #category : #tests }
ClyCompositeQueryTest >> testIsExecutedFromSingleScopeWhenSingleSubqueryIsBasedOnIt [

	query subqueries: { 
		self subqueryFromScopeOf: self class.
	}.
	
	self assert: query isExecutedFromSingleScope
]

{ #category : #tests }
ClyCompositeQueryTest >> testIsExecutedFromSingleScopeWhenSubqueriesAreFromDifferentScopesOfSameObject [

	query subqueries: { 
		self subqueryFromScope: ClyExampleScope of: self class.
		self subqueryFromScope: ClyExample2Scope of: self class.
	}.
	
	self assert: query isExecutedFromSingleScope
]

{ #category : #tests }
ClyCompositeQueryTest >> testNotExecutedFromMultipleScopeWhenAllSubqueriesAreFromEmptyScope [

	query subqueries: { 
		self subqueryFromScope: ClyExampleScope empty.
		self subqueryFromScope: ClyExample2Scope empty.
	}.
	
	self deny: query isExecutedFromMultipleScope
]

{ #category : #tests }
ClyCompositeQueryTest >> testNotExecutedFromMultipleScopeWhenSingleSubqueryIsBasedOnScopeOfSingleObject [

	query subqueries: { 
		self subqueryFromScopeOf: self class.
	}.
	
	self deny: query isExecutedFromMultipleScope
]

{ #category : #tests }
ClyCompositeQueryTest >> testNotExecutedFromMultipleScopeWhenSingleSubqueryIsFromEmptyScope [

	query subqueries: { 
		self subqueryFromScope: ClyExampleScope empty.
	}.
	
	self deny: query isExecutedFromMultipleScope
]

{ #category : #tests }
ClyCompositeQueryTest >> testNotExecutedFromMultipleScopeWhenSubqueriesAreFromDifferentScopesOfSameObject [

	query subqueries: { 
		self subqueryFromScope: ClyExampleScope of: self class.
		self subqueryFromScope: ClyExample2Scope of: self class.
	}.
	
	self deny: query isExecutedFromMultipleScope
]

{ #category : #tests }
ClyCompositeQueryTest >> testNotExecutedFromSingleScopeWhenAllSubqueriesAreFromEmptyScope [

	query subqueries: { 
		self subqueryFromScope: ClyExampleScope empty.
		self subqueryFromScope: ClyExample2Scope empty.
	}.
	
	self deny: query isExecutedFromSingleScope
]

{ #category : #tests }
ClyCompositeQueryTest >> testNotExecutedFromSingleScopeWhenSubqueriesAreFromScopesOfDifferentObjects [

	query subqueries: { 
		self subqueryFromScopeOf: self class.
		self subqueryFromScopeOf: self class superclass.
	}.
	
	self deny: query isExecutedFromSingleScope.
	
]
