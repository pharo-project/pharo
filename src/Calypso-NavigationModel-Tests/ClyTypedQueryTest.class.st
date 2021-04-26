Class {
	#name : #ClyTypedQueryTest,
	#superclass : #ClyQueryTest,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyTypedQueryTest class >> isAbstract [
	^self = ClyTypedQueryTest 
]

{ #category : #running }
ClyTypedQueryTest >> queryFromScope: scopeClass of: aBasisObject [
	query scope: (scopeClass of: aBasisObject in: environment).
	self executeQuery
]

{ #category : #running }
ClyTypedQueryTest >> queryFromScope: scopeClass ofAll: basisObjects [
	query scope: (scopeClass ofAll: basisObjects in: environment).
	self executeQuery
]

{ #category : #tests }
ClyTypedQueryTest >> testComparisonOfSimilarQueriesWithDifferentScopes [
	| query2 |
	query2 := self createQuery.
	query scope: (ClyScopeExample of: #basisObject1 in: environment).
	query2 scope: (ClyScopeExample of: #basisObject2 in: environment).
	self deny: query equals: query2.

	query2 scope: (ClyScopeExample2 of: #basisObject1 in: environment).
	self deny: query equals: query2
]

{ #category : #tests }
ClyTypedQueryTest >> testComparisonOfSimilarQueriesWithEqualScopes [
	| query2 |
	query2 := self createQuery.
	query scope: (ClyScopeExample of: #basisObject in: environment).
	query2 scope: (ClyScopeExample of: #basisObject in: environment).
	
	self assert: query equals: query2.
	self assert: query hash equals: query2 hash
]

{ #category : #tests }
ClyTypedQueryTest >> testConvertingAsRestrictedByAnotherScope [
	| newScope convertedQuery |
	newScope := ClyScopeExample2 of: #newBasis in: environment.
	
	convertedQuery := query restrictedByScope: newScope.
	
	self assert: convertedQuery class equals: query class.
	self assert: convertedQuery scope class equals: query scope class.
	self assert: convertedQuery scope basisObjects equals: #(newBasis) asSet
]

{ #category : #tests }
ClyTypedQueryTest >> testConvertingWithNewCompositeScope [
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
ClyTypedQueryTest >> testConvertingWithNewScope [
	| newScope convertedQuery |
	newScope := ClyScopeExample2 of: #someBasis in: environment.
	
	convertedQuery := query withScope: newScope.
	
	self assert: convertedQuery class equals: query class.
	self assert: convertedQuery scope equals: newScope
]

{ #category : #tests }
ClyTypedQueryTest >> testConvertingWithNewScopeBasis [
	| convertedQuery |
	
	convertedQuery := query withScopeOf: #(newBasis).
	
	self assert: convertedQuery class equals: query class.
	self assert: convertedQuery scope class equals: query scope class.
	self assert: convertedQuery scope basisObjects equals: #(newBasis) asSet
]

{ #category : #tests }
ClyTypedQueryTest >> testCreationOnMultipleScopeOfSameBasis [

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
ClyTypedQueryTest >> testCreationUnifiedInstance [

	| unifiedInstance |
	unifiedInstance := query asUnifiedInstance.
	
	self assert: unifiedInstance class equals: query class.
	self assert: unifiedInstance requiredResult equals: query requiredResult.
	self assert: unifiedInstance scope equals: ClyUnknownScope instance 
]

{ #category : #tests }
ClyTypedQueryTest >> testExecutesConcreteQueryClass [
	
	self assert: (query executesQuery: query class).
	self assert: (query executesQuery: query class superclass).
	
	self deny: (query executesQuery: ClyReturningScopeBasis2ExampleQuery)
]

{ #category : #tests }
ClyTypedQueryTest >> testForbidCreationOnScopeWhichNotSupportIt [

	[query scope: (ClyCompositeScope in: environment).
	self assert: false description: 'Query should be supported by scope'
	] onErrorDo: [].
	
]

{ #category : #tests }
ClyTypedQueryTest >> testForbidCreationOnScopeWithoutEnvironment [
	[ 
		query scope: (ClyScopeExample ofAll: {}).
		self assert: false description: 'Query should be only created on scope bound to concrete environment']
			onErrorDo: [  ]
]

{ #category : #tests }
ClyTypedQueryTest >> testIsExecutedFromConcreteScopeClass [

	self assert: (query isExecutedFromScope: query scope class).
	self assert: (query isExecutedFromScope: query scope class superclass).
	
	self deny: (query isExecutedFromScope: ClyScopeExample2).
]

{ #category : #tests }
ClyTypedQueryTest >> testIsExecutedFromEmptyScope [
	
	query scope: (ClyScopeExample ofAll: #(object1 object2) in: environment).
	self deny: query isExecutedFromEmptyScope.
	
	query scope: (ClyScopeExample emptyIn: environment).
	self assert: query isExecutedFromEmptyScope
]

{ #category : #tests }
ClyTypedQueryTest >> testIsExecutedFromMultipleScope [

	query scope: (ClyScopeExample ofAll: #(object1 object2) in: environment).
	self assert: query isExecutedFromMultipleScope.

	query scope: (ClyScopeExample of: #singleObject in: environment).
	self deny: query isExecutedFromMultipleScope.

	query scope: (ClyScopeExample emptyIn: environment).
	self deny: query isExecutedFromMultipleScope
]

{ #category : #tests }
ClyTypedQueryTest >> testIsExecutedFromScopeOfBasisObject [
	query scope: (ClyScopeExample of: #basis in: environment).

	self assert: (query isExecutedFromScopeOf: query scope basisObjects anyOne).
	self deny: (query isExecutedFromScopeOf: #someObject)
]

{ #category : #tests }
ClyTypedQueryTest >> testIsExecutedFromSingleScope [
	query scope: (ClyScopeExample ofAll: #(object1 object2) in: environment).
	self deny: query isExecutedFromSingleScope.

	query scope: (ClyScopeExample of: #singleObject in: environment).
	self assert: query isExecutedFromSingleScope.

	query scope: (ClyScopeExample emptyIn: environment).
	self deny: query isExecutedFromSingleScope
]
