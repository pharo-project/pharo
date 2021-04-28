Class {
	#name : #ClyMethodGroupProviderTest,
	#superclass : #ClyItemGroupProviderTest,
	#category : #'Calypso-SystemQueries-Tests-Domain'
}

{ #category : #testing }
ClyMethodGroupProviderTest class >> isAbstract [
	^self == ClyMethodGroupProviderTest 
]

{ #category : #running }
ClyMethodGroupProviderTest >> buildGroupsFor: aClass [
	| classScope |
	classScope := ClyClassScope of: aClass in: environment.
	builtGroups := groupProvider buildGroupsFrom: classScope
]

{ #category : #running }
ClyMethodGroupProviderTest >> classSampleWhichHasGroup [
	self subclassResponsibility 
]

{ #category : #tests }
ClyMethodGroupProviderTest >> testBuildingGroups [

	self buildGroupsFor: self classSampleWhichHasGroup.
	
	self assert: builtGroups notEmpty
]
