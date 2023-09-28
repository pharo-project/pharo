Class {
	#name : 'ClyMethodsInProtocolGroupProviderTest',
	#superclass : 'ClyMethodGroupProviderTest',
	#category : 'Calypso-SystemQueries-Tests-Domain',
	#package : 'Calypso-SystemQueries-Tests',
	#tag : 'Domain'
}

{ #category : 'running' }
ClyMethodsInProtocolGroupProviderTest >> classSampleWhichHasGroup [
	^ClyClass1FromP1Mock
]

{ #category : 'running' }
ClyMethodsInProtocolGroupProviderTest >> groupProviderClass [
	^ClyMethodsInProtocolGroupProvider
]

{ #category : 'tests' }
ClyMethodsInProtocolGroupProviderTest >> testCreateGroupsForEveryProtocol [

	| groups query |
	self buildGroupsFor: ClySubclassN1OfClass1FromP1Mock.

	groups := builtGroups select: [ :each | each isKindOf: ClyMethodsInProtocolGroup ].
	self assertCollection: (groups collect: [ :group | group protocol ]) hasSameElements: ClySubclassN1OfClass1FromP1Mock protocolNames.
	query := groups first methodQuery.
	self assert: query class equals: ClyMethodsInProtocolQuery.
	self assert: query scope equals: (ClyClassScope of: ClySubclassN1OfClass1FromP1Mock)
]
