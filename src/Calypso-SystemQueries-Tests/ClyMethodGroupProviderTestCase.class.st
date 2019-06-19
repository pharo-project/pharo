Class {
	#name : #ClyMethodGroupProviderTestCase,
	#superclass : #ClyItemGroupProviderTestCase,
	#category : #'Calypso-SystemQueries-Tests-Domain'
}

{ #category : #testing }
ClyMethodGroupProviderTestCase class >> isAbstract [
	^self == ClyMethodGroupProviderTestCase 
]

{ #category : #running }
ClyMethodGroupProviderTestCase >> buildGroupsFor: aClass [
	| classScope |
	classScope := ClyClassScope of: aClass in: environment.
	builtGroups := groupProvider buildGroupsFrom: classScope
]

{ #category : #running }
ClyMethodGroupProviderTestCase >> classSampleWhichHasGroup [
	self subclassResponsibility 
]

{ #category : #tests }
ClyMethodGroupProviderTestCase >> testBuildingGroups [

	self buildGroupsFor: self classSampleWhichHasGroup.
	
	self assert: builtGroups notEmpty
]
