Class {
	#name : 'ClyAllMethodGroupsQueryTest',
	#superclass : 'ClyTypedQueryTest',
	#category : 'Calypso-SystemQueries-Tests-Queries',
	#package : 'Calypso-SystemQueries-Tests',
	#tag : 'Queries'
}

{ #category : 'running' }
ClyAllMethodGroupsQueryTest >> createQuery [
	^ClyAllMethodGroupsQuery from: ClyClassScope of: Object in: environment
]

{ #category : 'running' }
ClyAllMethodGroupsQueryTest >> setUp [
	super setUp.

	environment addPlugin: ClyDefaultSystemEnvironmentPlugin new
]

{ #category : 'tests' }
ClyAllMethodGroupsQueryTest >> testCheckIfEmpty [
	| scope |
	scope := ClyInstanceSideScope of: ClyClass1FromP1Mock in: environment.
	query scope: scope.

	self deny: query checkEmptyResult
]

{ #category : 'tests' }
ClyAllMethodGroupsQueryTest >> testConvertingToNewMetaLevel [

	| newQuery |
	newQuery := query withMetaLevelScope: ClyInstanceSideScope.

	self assert: newQuery scope class equals: ClyInstanceSideScope.
	self assert: newQuery scope basisObjects equals: query scope basisObjects
]

{ #category : 'tests' }
ClyAllMethodGroupsQueryTest >> testFromSingleClass [

	self queryFromScope: ClyInstanceSideScope of: ClyClass1FromP1Mock.

	self assertCollection: (resultItems collect: #class) hasSameElements: {
			ClyAllMethodGroup.
			ClyMethodsInProtocolGroup }.
	self
		assertCollection: (resultItems
				 select: [ :each | each class = ClyMethodsInProtocolGroup ]
				 thenCollect: #protocol)
		hasSameElements: ClyClass1FromP1Mock protocolNames
]
