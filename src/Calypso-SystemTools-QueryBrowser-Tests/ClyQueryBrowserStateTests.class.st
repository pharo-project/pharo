Class {
	#name : #ClyQueryBrowserStateTests,
	#superclass : #ClyBrowserStateTestCase,
	#category : #'Calypso-SystemTools-QueryBrowser-Tests'
}

{ #category : #running }
ClyQueryBrowserStateTests >> createBrowser [
	^ ClyQueryBrowser on: environment scopes: {
			ClyBothMetaLevelClassScope of: ClyClass1FromP1 in: environment.
			ClyBothMetaLevelClassScope of: ClyClass2FromP1 in: environment}	"class with methods"
]

{ #category : #running }
ClyQueryBrowserStateTests >> restoreBrowserInstance [

	self snapshotState.
	
	navigationState applyTo: browser
]

{ #category : #running }
ClyQueryBrowserStateTests >> setUp [
	super setUp.
	
	browser showResultOf: (ClyAllMethods sortedFrom: browser queryScopes first)
]

{ #category : #tests }
ClyQueryBrowserStateTests >> testRestoredBrowserInstanceWithActiveScope [

	browser activeScope: browser queryScopes last.
	
	self restoreBrowserInstance.
	
	self assert: browser activeScope equals: browser queryScopes last
]

{ #category : #tests }
ClyQueryBrowserStateTests >> testRestoredBrowserInstanceWithMethodQuery [

	| query |
	query := ClyAllMethods sorted.
	browser systemQuery: query.
	
	self restoreBrowserInstance.
	
	self assert: browser systemQuery equals: query
]

{ #category : #tests }
ClyQueryBrowserStateTests >> testRestoredBrowserInstanceWithQueryScopes [

	self restoreBrowserInstance.
	self assert: navigationState queryScopes size equals: 2.
	
	self assert: browser queryScopes equals: navigationState queryScopes.
	
]

{ #category : #tests }
ClyQueryBrowserStateTests >> testRetrievesActiveScope [

	self snapshotState.
	
	self assert: navigationState activeScope equals: browser activeScope
]

{ #category : #tests }
ClyQueryBrowserStateTests >> testRetrievesMethodQuery [

	self snapshotState.
	
	self assert: navigationState systemQuery equals: browser systemQuery
]

{ #category : #tests }
ClyQueryBrowserStateTests >> testRetrievesQueryScopes [

	self snapshotState.
	
	self assert: navigationState queryScopes equals: browser queryScopes
]
