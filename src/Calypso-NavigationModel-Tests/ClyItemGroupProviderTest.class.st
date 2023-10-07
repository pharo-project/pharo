Class {
	#name : 'ClyItemGroupProviderTest',
	#superclass : 'ClyAbstractNavigationEnvironmentTest',
	#instVars : [
		'groupProvider',
		'builtGroups'
	],
	#category : 'Calypso-NavigationModel-Tests',
	#package : 'Calypso-NavigationModel-Tests'
}

{ #category : 'testing' }
ClyItemGroupProviderTest class >> isAbstract [
	^self == ClyItemGroupProviderTest
]

{ #category : 'running' }
ClyItemGroupProviderTest >> groupProviderClass [
	self subclassResponsibility
]

{ #category : 'running' }
ClyItemGroupProviderTest >> setUp [
	super setUp.
	groupProvider := self groupProviderClass new.
	builtGroups := OrderedCollection new
]
