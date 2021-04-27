Class {
	#name : #ClyTaggedClassGroupProviderTest,
	#superclass : #ClyClassGroupProviderTest,
	#category : #'Calypso-SystemQueries-Tests-Domain'
}

{ #category : #running }
ClyTaggedClassGroupProviderTest >> groupProviderClass [
	^ClyTaggedClassGroupProvider 
]

{ #category : #running }
ClyTaggedClassGroupProviderTest >> packageSampleWhichHasGroup [
	^ClyClass7WithTag1FromP5Mock package "P5WithTags"
]

{ #category : #tests }
ClyTaggedClassGroupProviderTest >> testCreatesGroupsForEveryTag [

	self buildGroupsFor: ClyClass7WithTag1FromP5Mock package.
	
	self assert: builtGroups size equals: 2.
	self assert: (builtGroups collect: [:each | each tag]) sorted asArray equals: #(Tag1 Tag2)
]

{ #category : #tests }
ClyTaggedClassGroupProviderTest >> testFromTwoSamePackages [

	self buildGroupsForAll: {ClyClass7WithTag1FromP5Mock package. ClyClass7WithTag1FromP5Mock package}.
	
	self assert: builtGroups size equals: 2.
	self assert: (builtGroups collect: [:each | each tag]) sorted asArray equals: #(Tag1 Tag2)
]
