Class {
	#name : #ClyQueryBrowserStateTest,
	#superclass : #ClyBrowserStateTestCase,
	#category : #'Calypso-SystemTools-QueryBrowser-Tests'
}

{ #category : #running }
ClyQueryBrowserStateTest >> createBrowser [
	^ ClyQueryBrowser on: environment scopes: {
			ClyBothMetaLevelClassScope of: ClyClass1FromP1 in: environment.
			ClyBothMetaLevelClassScope of: ClyClass2FromP1 in: environment}	"class with methods"
]

{ #category : #running }
ClyQueryBrowserStateTest >> restoreBrowserInstance [

	self snapshotState.
	
	navigationState applyTo: browser
]

{ #category : #running }
ClyQueryBrowserStateTest >> setUp [
	super setUp.
	
	browser showResultOf: (ClyAllMethodsQuery sortedFrom: browser queryScopes first)
]

{ #category : #tests }
ClyQueryBrowserStateTest >> testRestoredBrowserInstanceWithActiveScope [

	browser activeScope: browser queryScopes last.
	
	self restoreBrowserInstance.
	
	self assert: browser activeScope equals: browser queryScopes last
]

{ #category : #tests }
ClyQueryBrowserStateTest >> testRestoredBrowserInstanceWithExpectedFlatQueryResult [

	self restoreBrowserInstance.
	
	self assert: navigationState expectedFlatQueryResult notNil.
	self assert: browser expectedFlatQueryResult equals: navigationState expectedFlatQueryResult
]

{ #category : #tests }
ClyQueryBrowserStateTest >> testRestoredBrowserInstanceWithMethodQuery [

	| query |
	query := ClyAllMethodsQuery sorted.
	browser systemQuery: query.
	
	self restoreBrowserInstance.
	
	self assert: browser systemQuery equals: query
]

{ #category : #tests }
ClyQueryBrowserStateTest >> testRestoredBrowserInstanceWithQueryScopes [

	self restoreBrowserInstance.
	self assert: navigationState queryScopes size equals: 2.
	
	self assert: browser queryScopes equals: navigationState queryScopes.
	
]

{ #category : #tests }
ClyQueryBrowserStateTest >> testRetrievesActiveScope [

	self snapshotState.
	
	self assert: navigationState activeScope equals: browser activeScope
]

{ #category : #tests }
ClyQueryBrowserStateTest >> testRetrievesExpectedFlatQueryResult [

	self snapshotState.
	
	self assert: navigationState expectedFlatQueryResult equals: browser expectedFlatQueryResult
]

{ #category : #tests }
ClyQueryBrowserStateTest >> testRetrievesMethodQuery [

	self snapshotState.
	
	self assert: navigationState systemQuery equals: browser systemQuery
]

{ #category : #tests }
ClyQueryBrowserStateTest >> testRetrievesQueryScopes [

	self snapshotState.
	
	self assert: navigationState queryScopes equals: browser queryScopes
]
