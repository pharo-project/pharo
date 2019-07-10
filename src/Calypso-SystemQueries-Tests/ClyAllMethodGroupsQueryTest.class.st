Class {
	#name : #ClyAllMethodGroupsQueryTest,
	#superclass : #ClyTypedQueryTestCase,
	#category : #'Calypso-SystemQueries-Tests-Queries'
}

{ #category : #running }
ClyAllMethodGroupsQueryTest >> createQuery [
	^ClyAllMethodGroupsQuery from: ClyClassScope of: Object in: environment
]

{ #category : #running }
ClyAllMethodGroupsQueryTest >> setUp [
	super setUp.
	
	environment addPlugin: ClyDefaultSystemEnvironmentPlugin new
]

{ #category : #tests }
ClyAllMethodGroupsQueryTest >> testCheckIfEmpty [
	| scope |
	scope := ClyInstanceSideScope of: ClyClass1FromP1 in: environment.
	query scope: scope.
	
	self deny: query checkEmptyResult
]

{ #category : #tests }
ClyAllMethodGroupsQueryTest >> testConvertingToNewMetaLevel [

	| newQuery |
	newQuery := query withMetaLevelScope: ClyInstanceSideScope.
	
	self assert: newQuery scope class equals: ClyInstanceSideScope.
	self assert: newQuery scope basisObjects equals: query scope basisObjects.
]

{ #category : #tests }
ClyAllMethodGroupsQueryTest >> testFromSingleClass [

	self queryFromScope: ClyInstanceSideScope of: ClyClass1FromP1.
	
	self assert: (resultItems collect: #class as: Set) equals: { ClyAllMethodGroup. ClyTaggedMethodGroup} asSet.
	self 
		assert: (resultItems select:[:each | each class = ClyTaggedMethodGroup] thenCollect: #tag) asSet
		equals: ClyClass1FromP1 tagsForMethods asSet
]
