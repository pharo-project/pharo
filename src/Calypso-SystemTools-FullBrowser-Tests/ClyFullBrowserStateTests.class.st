Class {
	#name : #ClyFullBrowserStateTests,
	#superclass : #ClyBrowserStateTestCase,
	#category : #'Calypso-SystemTools-FullBrowser-Tests'
}

{ #category : #running }
ClyFullBrowserStateTests >> restoreBrowserInstance [

	self snapshotState.
	
	navigationState applyTo: browser
]

{ #category : #running }
ClyFullBrowserStateTests >> setUp [
	super setUp.
	
	browser prepareInitialState
]

{ #category : #tests }
ClyFullBrowserStateTests >> testRestoredBrowserInstanceWithMetaLevelScope [
	
	browser metaLevelScope: #testScope.
	
	self restoreBrowserInstance.
	
	self assert: browser metaLevelScope equals: #testScope
]

{ #category : #tests }
ClyFullBrowserStateTests >> testRestoredBrowserInstanceWithMethodGroupQuery [
	
	browser methodGroupQuery: #testQuery.
	
	self restoreBrowserInstance.
	
	self assert: browser methodGroupQuery equals: #testQuery
]

{ #category : #tests }
ClyFullBrowserStateTests >> testRestoredBrowserInstanceWithSystemScope [
	
	browser systemScope: #testScope.
	
	self restoreBrowserInstance.
	
	self assert: browser systemScope equals: #testScope
]

{ #category : #tests }
ClyFullBrowserStateTests >> testRetrievesMetaLevelScope [

	self snapshotState.
	
	self assert: navigationState metaLevelScope equals: browser metaLevelScope
]

{ #category : #tests }
ClyFullBrowserStateTests >> testRetrievesMethodGroupContent [

	self snapshotState.
	
	self assert: navigationState methodGroupQuery equals: browser methodGroupQuery
]

{ #category : #tests }
ClyFullBrowserStateTests >> testRetrievesSystemScope [

	self snapshotState.
	
	self assert: navigationState systemScope equals: environment systemScope
]
