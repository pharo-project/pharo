Class {
	#name : #ClyTaggedClassGroupProviderTest,
	#superclass : #ClyClassGroupProviderTestCase,
	#category : #'Calypso-SystemQueries-Tests-Domain'
}

{ #category : #running }
ClyTaggedClassGroupProviderTest >> groupProviderClass [
	^ClyTaggedClassGroupProvider 
]

{ #category : #running }
ClyTaggedClassGroupProviderTest >> packageSampleWhichHasGroup [
	^ClyClass7WithTag1FromP5 package "P5WithTags"
]

{ #category : #tests }
ClyTaggedClassGroupProviderTest >> testCreatesGroupsForEveryTag [

	self buildGroupsFor: ClyClass7WithTag1FromP5 package.
	
	self assert: builtGroups size equals: 2.
	self assert: (builtGroups collect: #tag) sorted asArray equals: #(Tag1 Tag2)
]

{ #category : #tests }
ClyTaggedClassGroupProviderTest >> testFromTwoSamePackages [

	self buildGroupsForAll: {ClyClass7WithTag1FromP5 package. ClyClass7WithTag1FromP5 package}.
	
	self assert: builtGroups size equals: 2.
	self assert: (builtGroups collect: #tag) sorted asArray equals: #(Tag1 Tag2)
]
