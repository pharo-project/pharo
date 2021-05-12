Class {
	#name : #ClyNavigationHistoryTest,
	#superclass : #TestCase,
	#instVars : [
		'environment',
		'browser',
		'navigationState'
	],
	#category : #'Calypso-SystemTools-FullBrowser-Tests'
}

{ #category : #testing }
ClyNavigationHistoryTest class >> isAbstract [
	^self == ClyNavigationHistoryTest
]

{ #category : #running }
ClyNavigationHistoryTest >> createBrowser [
	^ClyFullBrowserMorph new
]

{ #category : #running }
ClyNavigationHistoryTest >> setUp [
	super setUp.
	
	environment := ClyNavigationEnvironment overSystemEnvironment: ClySystemEnvironment currentImage.
	environment detachFromSystem "to ensure no subscriptions".
	
	browser := self createBrowser.
	browser navigationEnvironment: environment.
	browser plugins removeAll.
]

{ #category : #running }
ClyNavigationHistoryTest >> snapshotState [
	self subclassResponsibility 
]
