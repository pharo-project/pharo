Class {
	#name : #ClyNavigationEnvironmentTestCase,
	#superclass : #TestCase,
	#instVars : [
		'environment'
	],
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyNavigationEnvironmentTestCase class >> isAbstract [
	^self = ClyNavigationEnvironmentTestCase 
]

{ #category : #running }
ClyNavigationEnvironmentTestCase >> setUp [
	super setUp.
	self setUpEnvironment 
]

{ #category : #running }
ClyNavigationEnvironmentTestCase >> setUpEnvironment [
	environment := ClyNavigationEnvironment new
]
