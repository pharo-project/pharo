Class {
	#name : #ClyTypedQueryTestCase,
	#superclass : #ClyQueryTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyTypedQueryTestCase class >> isAbstract [
	^self = ClyTypedQueryTestCase 
]

{ #category : #running }
ClyTypedQueryTestCase >> queryFromScope: scopeClass of: aBasisObject [
	query scope: (scopeClass of: aBasisObject in: environment).
	self executeQuery
]

{ #category : #running }
ClyTypedQueryTestCase >> queryFromScope: scopeClass ofAll: basisObjects [
	query scope: (scopeClass ofAll: basisObjects in: environment).
	self executeQuery
]

{ #category : #tests }
ClyTypedQueryTestCase >> testComparisonOfSimilarQueriesWithDifferentScopes [
	| query2 |
	query2 := self createQuery.
	query scope: (ClyScopeExample of: #basisObject1 in: environment).
	query2 scope: (ClyScopeExample of: #basisObject2 in: environment).
	self deny: query equals: query2.

	query2 scope: (ClyScopeExample2 of: #basisObject1 in: environment).
	self deny: query equals: query2
]

{ #category : #tests }
ClyTypedQueryTestCase >> testComparisonOfSimilarQueriesWithEqualScopes [
	| query2 |
	query2 := self createQuery.
	query scope: (ClyScopeExample of: #basisObject in: environment).
	query2 scope: (ClyScopeExample of: #basisObject in: environment).
	
	self assert: query equals: query2.
	self assert: query hash equals: query2 hash
]

{ #category : #tests }
ClyTypedQueryTestCase >> testConvertingAsRestrictedByAnotherScope [
	| newScope convertedQuery |
	newScope := ClyScopeExample2 of: #newBasis in: environment.
	
	convertedQuery := query restrictedByScope: newScope.
	
	self assert: convertedQuery class equals: query class.
	self assert: convertedQuery scope class equals: query scope class.
	self assert: convertedQuery scope basisObjects equals: #(newBasis) asSet
]

{ #category : #tests }
ClyTypedQueryTestCase >> testConvertingWithNewCompositeScope [
	| newScope convertedQuery |
	newScope := ClyCompositeScope on: {
		ClyScopeExample of: #someBasis1 in: environment.
		ClyScopeExample2 of: #someBasis2 in: environment}.
		
	convertedQuery := query withScope: newScope.
	
	self assert: convertedQuery class equals: ClyUnionQuery.
	self
		assert: (convertedQuery subqueries collect: #class as: Set)
		equals: {query class} asSet.
	self
		assert: (convertedQuery subqueries collect: #scope as: Set)
		equals: newScope subscopes asSet
]

{ #category : #tests }
ClyTypedQueryTestCase >> testConvertingWithNewScope [
	| newScope convertedQuery |
	newScope := ClyScopeExample2 of: #someBasis in: environment.
	
	convertedQuery := query withScope: newScope.
	
	self assert: convertedQuery class equals: query class.
	self assert: convertedQuery scope equals: newScope
]

{ #category : #tests }
ClyTypedQueryTestCase >> testConvertingWithNewScopeBasis [
	| convertedQuery |
	
	convertedQuery := query withScopeOf: #(newBasis).
	
	self assert: convertedQuery class equals: query class.
	self assert: convertedQuery scope class equals: query scope class.
	self assert: convertedQuery scope basisObjects equals: #(newBasis) asSet
]

{ #category : #tests }
ClyTypedQueryTestCase >> testCreationOnMultipleScopeOfSameBasis [

	| queryClass |
	queryClass := query class.
	query := queryClass 
		fromAll: {ClyScopeExample. ClyScopeExample2 } of: Array in: environment.
	
	self assert: query class equals: ClyUnionQuery.
	self assert: query subqueries equals: {
		queryClass from: ClyScopeExample of: Array in: environment.
		queryClass from: ClyScopeExample2 of: Array in: environment.
	} asSet.
]

{ #category : #tests }
ClyTypedQueryTestCase >> testCreationUnifiedInstance [

	| unifiedInstance |
	unifiedInstance := query asUnifiedInstance.
	
	self assert: unifiedInstance class equals: query class.
	self assert: unifiedInstance requiredResult equals: query requiredResult.
	self assert: unifiedInstance scope equals: ClyUnknownScope instance 
]

{ #category : #tests }
ClyTypedQueryTestCase >> testExecutesConcreteQueryClass [
	
	self assert: (query executesQuery: query class).
	self assert: (query executesQuery: query class superclass).
	
	self deny: (query executesQuery: ClyQueryExampleReturningScopeBasis2)
]

{ #category : #tests }
ClyTypedQueryTestCase >> testForbidCreationOnScopeWhichNotSupportIt [

	[query scope: (ClyCompositeScope in: environment).
	self assert: false description: 'Query should be supported by scope'
	] ifError: [].
	
]

{ #category : #tests }
ClyTypedQueryTestCase >> testForbidCreationOnScopeWithoutEnvironment [
	[ 
		query scope: (ClyScopeExample ofAll: {}).
		self assert: false description: 'Query should be only created on scope bound to concrete environment']
			ifError: [  ]
]

{ #category : #tests }
ClyTypedQueryTestCase >> testIsExecutedFromConcreteScopeClass [

	self assert: (query isExecutedFromScope: query scope class).
	self assert: (query isExecutedFromScope: query scope class superclass).
	
	self deny: (query isExecutedFromScope: ClyScopeExample2).
]

{ #category : #tests }
ClyTypedQueryTestCase >> testIsExecutedFromEmptyScope [
	
	query scope: (ClyScopeExample ofAll: #(object1 object2) in: environment).
	self deny: query isExecutedFromEmptyScope.
	
	query scope: (ClyScopeExample emptyIn: environment).
	self assert: query isExecutedFromEmptyScope
]

{ #category : #tests }
ClyTypedQueryTestCase >> testIsExecutedFromMultipleScope [

	query scope: (ClyScopeExample ofAll: #(object1 object2) in: environment).
	self assert: query isExecutedFromMultipleScope.

	query scope: (ClyScopeExample of: #singleObject in: environment).
	self deny: query isExecutedFromMultipleScope.

	query scope: (ClyScopeExample emptyIn: environment).
	self deny: query isExecutedFromMultipleScope
]

{ #category : #tests }
ClyTypedQueryTestCase >> testIsExecutedFromScopeOfBasisObject [
	query scope: (ClyScopeExample of: #basis in: environment).

	self assert: (query isExecutedFromScopeOf: query scope basisObjects anyOne).
	self deny: (query isExecutedFromScopeOf: #someObject)
]

{ #category : #tests }
ClyTypedQueryTestCase >> testIsExecutedFromSingleScope [
	query scope: (ClyScopeExample ofAll: #(object1 object2) in: environment).
	self deny: query isExecutedFromSingleScope.

	query scope: (ClyScopeExample of: #singleObject in: environment).
	self assert: query isExecutedFromSingleScope.

	query scope: (ClyScopeExample emptyIn: environment).
	self deny: query isExecutedFromSingleScope
]
