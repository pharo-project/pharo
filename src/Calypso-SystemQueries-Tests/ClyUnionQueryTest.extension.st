Extension { #name : 'ClyUnionQueryTest' }

{ #category : '*Calypso-SystemQueries-Tests' }
ClyUnionQueryTest >> testConvertingToNewMetaLevel [

	| newQuery scopes |
	self query subqueries: {
			(ClyAllMethodGroupsQuery from: ClyClassScope of: Object in: self environment).
			(ClyAllMethodGroupsQuery from: ClySubclassScope of: Array in: self environment) }.

	newQuery := self query withMetaLevelScope: ClyInstanceSideScope.
	self assert: newQuery class equals: self queryClass.
	self
		assert: (newQuery subqueries collect: [ :each | each class ])
		equals: (self query subqueries collect: [ :each | each class ]).
	self assert: newQuery requiredResult identicalTo: self query requiredResult.
	scopes := newQuery subqueries collect: [ :each | each scope ] as: Set.
	self assert: scopes equals: {
			(ClyInstanceSideScope of: Object).
			(ClySubclassScope of: Array localScope: ClyInstanceSideScope) } asSet
]
