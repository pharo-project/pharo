Class {
	#name : 'ClyClassGroupProviderTest',
	#superclass : 'ClyItemGroupProviderTest',
	#category : 'Calypso-SystemQueries-Tests-Domain',
	#package : 'Calypso-SystemQueries-Tests',
	#tag : 'Domain'
}

{ #category : 'testing' }
ClyClassGroupProviderTest class >> isAbstract [
	^self = ClyClassGroupProviderTest
]

{ #category : 'running' }
ClyClassGroupProviderTest >> buildGroupsFor: aPackage [
	| classScope |
	classScope := ClyPackageScope of: aPackage in: environment.
	groupProvider
		packageTagssIn: classScope
		do: [ :each | builtGroups add: each ]
]

{ #category : 'running' }
ClyClassGroupProviderTest >> buildGroupsForAll: packages [
	| classScope |
	classScope := ClyPackageScope ofAll: packages in: environment.
	groupProvider
		packageTagssIn: classScope
		do: [ :each | builtGroups add: each ]
]

{ #category : 'running' }
ClyClassGroupProviderTest >> packageSampleWhichHasGroup [
	self subclassResponsibility
]

{ #category : 'tests' }
ClyClassGroupProviderTest >> testBuildingGroups [

	self buildGroupsFor: self packageSampleWhichHasGroup.

	self assert: builtGroups notEmpty
]
