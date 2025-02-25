Class {
	#name : 'ClyAbstractNavigationEnvironmentTest',
	#superclass : 'TestCase',
	#instVars : [
		'environment'
	],
	#category : 'Calypso-NavigationModel-Tests',
	#package : 'Calypso-NavigationModel-Tests'
}

{ #category : 'testing' }
ClyAbstractNavigationEnvironmentTest class >> isAbstract [
	^self = ClyAbstractNavigationEnvironmentTest
]

{ #category : 'running' }
ClyAbstractNavigationEnvironmentTest >> setUp [
	super setUp.
	self setUpEnvironment
]

{ #category : 'running' }
ClyAbstractNavigationEnvironmentTest >> setUpEnvironment [
	environment := ClyNavigationEnvironment new
]
