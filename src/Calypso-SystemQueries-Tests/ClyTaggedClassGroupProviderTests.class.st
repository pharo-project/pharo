Class {
	#name : #ClyTaggedClassGroupProviderTests,
	#superclass : #ClyClassGroupProviderTestCase,
	#category : #'Calypso-SystemQueries-Tests-Domain'
}

{ #category : #running }
ClyTaggedClassGroupProviderTests >> groupProviderClass [
	^ClyTaggedClassGroupProvider 
]

{ #category : #running }
ClyTaggedClassGroupProviderTests >> packageSampleWhichHasGroup [
	^ClyClass7WithTag1FromP5 package "P5WithTags"
]

{ #category : #tests }
ClyTaggedClassGroupProviderTests >> testCreatesGroupsForEveryTag [

	self buildGroupsFor: ClyClass7WithTag1FromP5 package.
	
	self assert: builtGroups size equals: 2.
	self assert: (builtGroups collect: #tag) sorted asArray equals: #(Tag1 Tag2)
]

{ #category : #tests }
ClyTaggedClassGroupProviderTests >> testFromTwoSamePackages [

	self buildGroupsForAll: {ClyClass7WithTag1FromP5 package. ClyClass7WithTag1FromP5 package}.
	
	self assert: builtGroups size equals: 2.
	self assert: (builtGroups collect: #tag) sorted asArray equals: #(Tag1 Tag2)
]
