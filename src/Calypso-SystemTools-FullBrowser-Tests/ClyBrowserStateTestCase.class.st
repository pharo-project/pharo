Class {
	#name : #ClyBrowserStateTestCase,
	#superclass : #ClyNavigationHistoryTestCase,
	#category : #'Calypso-SystemTools-FullBrowser-Tests'
}

{ #category : #testing }
ClyBrowserStateTestCase class >> isAbstract [
	^self == ClyBrowserStateTestCase
]

{ #category : #running }
ClyBrowserStateTestCase >> snapshotState [

	navigationState := browser snapshotState.
		
]

{ #category : #tests }
ClyBrowserStateTestCase >> testRetrievesStateOfAllNavigationViews [

	self snapshotState.
	
	self assert: navigationState viewStates size equals: browser navigationViews size
]

{ #category : #tests }
ClyBrowserStateTestCase >> testSelectedTabsAreNotBuild [

	| tab |
	self snapshotState.
	
	tab := navigationState selectedTabs first.
	self deny: tab isBuilt
]

{ #category : #tests }
ClyBrowserStateTestCase >> testSelectedTabsWithoutBrowser [

	| tab |
	self snapshotState.
	
	tab := navigationState selectedTabs first.
	self assert: tab browser isNil
]

{ #category : #tests }
ClyBrowserStateTestCase >> testSelectedTabsWithoutContext [

	| tab |
	self snapshotState.
	
	tab := navigationState selectedTabs first.
	self assert: tab context isNil
]

{ #category : #tests }
ClyBrowserStateTestCase >> testSelectedTabsWithoutOwner [

	| tab |
	self snapshotState.
	
	tab := navigationState selectedTabs first.
	self assert: tab owner isNil
]
