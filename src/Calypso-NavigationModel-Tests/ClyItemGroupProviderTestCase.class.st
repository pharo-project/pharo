Class {
	#name : #ClyItemGroupProviderTestCase,
	#superclass : #ClyNavigationEnvironmentTestCase,
	#instVars : [
		'groupProvider',
		'builtGroups'
	],
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyItemGroupProviderTestCase class >> isAbstract [
	^self == ClyItemGroupProviderTestCase 
]

{ #category : #running }
ClyItemGroupProviderTestCase >> groupProviderClass [
	self subclassResponsibility 
]

{ #category : #running }
ClyItemGroupProviderTestCase >> setUp [
	super setUp.
	groupProvider := self groupProviderClass new.
	builtGroups := OrderedCollection new.
]
