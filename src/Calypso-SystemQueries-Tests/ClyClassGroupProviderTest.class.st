Class {
	#name : #ClyClassGroupProviderTest,
	#superclass : #ClyItemGroupProviderTest,
	#category : #'Calypso-SystemQueries-Tests-Domain'
}

{ #category : #testing }
ClyClassGroupProviderTest class >> isAbstract [
	^self = ClyClassGroupProviderTest 
]

{ #category : #running }
ClyClassGroupProviderTest >> buildGroupsFor: aPackage [
	| classScope |
	classScope := ClyPackageScope of: aPackage in: environment.
	groupProvider
		classGroupsIn: classScope
		do: [ :each | builtGroups add: each ]
]

{ #category : #running }
ClyClassGroupProviderTest >> buildGroupsForAll: packages [
	| classScope |
	classScope := ClyPackageScope ofAll: packages in: environment.
	groupProvider
		classGroupsIn: classScope
		do: [ :each | builtGroups add: each ]
]

{ #category : #running }
ClyClassGroupProviderTest >> packageSampleWhichHasGroup [
	self subclassResponsibility 
]

{ #category : #tests }
ClyClassGroupProviderTest >> testBuildingGroups [

	self buildGroupsFor: self packageSampleWhichHasGroup.
	
	self assert: builtGroups notEmpty
]
