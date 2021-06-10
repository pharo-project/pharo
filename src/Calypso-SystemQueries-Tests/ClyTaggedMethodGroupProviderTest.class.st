Class {
	#name : #ClyTaggedMethodGroupProviderTest,
	#superclass : #ClyMethodGroupProviderTest,
	#category : #'Calypso-SystemQueries-Tests-Domain'
}

{ #category : #running }
ClyTaggedMethodGroupProviderTest >> classSampleWhichHasGroup [
	^ClyClass1FromP1Mock 
]

{ #category : #running }
ClyTaggedMethodGroupProviderTest >> groupProviderClass [
	^ClyTaggedMethodGroupProvider
]

{ #category : #tests }
ClyTaggedMethodGroupProviderTest >> testCreateGroupsForEveryMethodTag [
	| groups query |
	
	self buildGroupsFor: ClySubclassN1OfClass1FromP1Mock.
	
	groups := builtGroups select: [ :each | each isKindOf: ClyTaggedMethodGroup ].
	self
		assert: (groups collect: #tag as: Set)
		equals: ClySubclassN1OfClass1FromP1Mock tagsForMethods asSet.
	query := groups first methodQuery.
	self assert: query class equals: ClyTaggedMethodsQuery.
	self assert: query scope equals: (ClyClassScope of: ClySubclassN1OfClass1FromP1Mock)
]
