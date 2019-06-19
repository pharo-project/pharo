Class {
	#name : #ClyClassGroupProviderTestCase,
	#superclass : #ClyItemGroupProviderTestCase,
	#category : #'Calypso-SystemQueries-Tests-Domain'
}

{ #category : #testing }
ClyClassGroupProviderTestCase class >> isAbstract [
	^self = ClyClassGroupProviderTestCase 
]

{ #category : #running }
ClyClassGroupProviderTestCase >> buildGroupsFor: aPackage [
	| classScope |
	classScope := ClyPackageScope of: aPackage in: environment.
	groupProvider
		classGroupsIn: classScope
		do: [ :each | builtGroups add: each ]
]

{ #category : #running }
ClyClassGroupProviderTestCase >> buildGroupsForAll: packages [
	| classScope |
	classScope := ClyPackageScope ofAll: packages in: environment.
	groupProvider
		classGroupsIn: classScope
		do: [ :each | builtGroups add: each ]
]

{ #category : #running }
ClyClassGroupProviderTestCase >> packageSampleWhichHasGroup [
	self subclassResponsibility 
]

{ #category : #tests }
ClyClassGroupProviderTestCase >> testBuildingGroups [

	self buildGroupsFor: self packageSampleWhichHasGroup.
	
	self assert: builtGroups notEmpty
]
