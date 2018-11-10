Class {
	#name : #ClyTaggedMethodGroupProviderTests,
	#superclass : #ClyMethodGroupProviderTestCase,
	#category : #'Calypso-SystemQueries-Tests-Domain'
}

{ #category : #running }
ClyTaggedMethodGroupProviderTests >> classSampleWhichHasGroup [
	^ClyClass1FromP1 
]

{ #category : #running }
ClyTaggedMethodGroupProviderTests >> groupProviderClass [
	^ClyTaggedMethodGroupProvider
]

{ #category : #tests }
ClyTaggedMethodGroupProviderTests >> testCreateGroupsForEveryMethodTag [
	| groups query |
	
	self buildGroupsFor: ClySubclassN1OfClass1FromP1.
	
	groups := builtGroups select: [ :each | each isKindOf: ClyTaggedMethodGroup ].
	self
		assert: (groups collect: #tag as: Set)
		equals: ClySubclassN1OfClass1FromP1 tagsForMethods asSet.
	query := groups first methodQuery.
	self assert: query class equals: ClyTaggedMethods.
	self assert: query scope equals: (ClyClassScope of: ClySubclassN1OfClass1FromP1)
]
