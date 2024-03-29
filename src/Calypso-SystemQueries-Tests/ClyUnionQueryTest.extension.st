Extension { #name : 'ClyUnionQueryTest' }

{ #category : '*Calypso-SystemQueries-Tests' }
ClyUnionQueryTest >> testConvertingToNewMetaLevel [
	| newQuery scopes |
	query
		subqueries:
			{(ClyAllMethodGroupsQuery from: ClyClassScope of: Object in: environment).
			(ClyAllMethodGroupsQuery from: ClySubclassScope of: Array in: environment)}.

	newQuery := query withMetaLevelScope: ClyInstanceSideScope.
	self assert: newQuery class equals: self queryClass.
	self assert: (newQuery subqueries collect: [:each | each class]) equals: (query subqueries collect: [:each | each class]).
	self assert: newQuery requiredResult identicalTo: query requiredResult.
	scopes := newQuery subqueries collect: [ :each | each scope ] as: Set.
	self assert: scopes equals: {(ClyInstanceSideScope of: Object) . (ClySubclassScope of: Array localScope: ClyInstanceSideScope)} asSet
]
