Class {
	#name : #ClyAbsractNavigationEnvironmentTest,
	#superclass : #TestCase,
	#instVars : [
		'environment'
	],
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #testing }
ClyAbsractNavigationEnvironmentTest class >> isAbstract [
	^self = ClyAbsractNavigationEnvironmentTest 
]

{ #category : #running }
ClyAbsractNavigationEnvironmentTest >> setUp [
	super setUp.
	self setUpEnvironment 
]

{ #category : #running }
ClyAbsractNavigationEnvironmentTest >> setUpEnvironment [
	environment := ClyNavigationEnvironment new
]
