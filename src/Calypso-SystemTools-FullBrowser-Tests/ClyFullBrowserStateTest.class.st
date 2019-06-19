Class {
	#name : #ClyFullBrowserStateTest,
	#superclass : #ClyBrowserStateTestCase,
	#category : #'Calypso-SystemTools-FullBrowser-Tests'
}

{ #category : #running }
ClyFullBrowserStateTest >> restoreBrowserInstance [

	self snapshotState.
	
	navigationState applyTo: browser
]

{ #category : #running }
ClyFullBrowserStateTest >> setUp [
	super setUp.
	
	browser prepareInitialState
]

{ #category : #tests }
ClyFullBrowserStateTest >> testRestoredBrowserInstanceWithMetaLevelScope [
	
	browser metaLevelScope: #testScope.
	
	self restoreBrowserInstance.
	
	self assert: browser metaLevelScope equals: #testScope
]

{ #category : #tests }
ClyFullBrowserStateTest >> testRestoredBrowserInstanceWithMethodGroupQuery [
	
	browser methodGroupQuery: #testQuery.
	
	self restoreBrowserInstance.
	
	self assert: browser methodGroupQuery equals: #testQuery
]

{ #category : #tests }
ClyFullBrowserStateTest >> testRestoredBrowserInstanceWithSystemScope [
	
	browser systemScope: #testScope.
	
	self restoreBrowserInstance.
	
	self assert: browser systemScope equals: #testScope
]

{ #category : #tests }
ClyFullBrowserStateTest >> testRetrievesMetaLevelScope [

	self snapshotState.
	
	self assert: navigationState metaLevelScope equals: browser metaLevelScope
]

{ #category : #tests }
ClyFullBrowserStateTest >> testRetrievesMethodGroupContent [

	self snapshotState.
	
	self assert: navigationState methodGroupQuery equals: browser methodGroupQuery
]

{ #category : #tests }
ClyFullBrowserStateTest >> testRetrievesSystemScope [

	self snapshotState.
	
	self assert: navigationState systemScope equals: environment systemScope
]
