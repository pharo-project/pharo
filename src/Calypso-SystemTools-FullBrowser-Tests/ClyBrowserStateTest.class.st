Class {
	#name : #ClyBrowserStateTest,
	#superclass : #ClyNavigationHistoryTest,
	#category : #'Calypso-SystemTools-FullBrowser-Tests'
}

{ #category : #testing }
ClyBrowserStateTest class >> isAbstract [
	^self == ClyBrowserStateTest
]

{ #category : #running }
ClyBrowserStateTest >> snapshotState [

	navigationState := browser snapshotState.
		
]

{ #category : #tests }
ClyBrowserStateTest >> testRetrievesStateOfAllNavigationViews [

	self snapshotState.
	
	self assert: navigationState viewStates size equals: browser navigationViews size
]

{ #category : #tests }
ClyBrowserStateTest >> testSelectedTabsAreNotBuild [

	| tab |
	self snapshotState.
	
	tab := navigationState selectedTabs first.
	self deny: tab isBuilt
]

{ #category : #tests }
ClyBrowserStateTest >> testSelectedTabsWithoutBrowser [

	| tab |
	self snapshotState.
	
	tab := navigationState selectedTabs first.
	self assert: tab browser isNil
]

{ #category : #tests }
ClyBrowserStateTest >> testSelectedTabsWithoutContext [

	| tab |
	self snapshotState.
	
	tab := navigationState selectedTabs first.
	self assert: tab context isNil
]

{ #category : #tests }
ClyBrowserStateTest >> testSelectedTabsWithoutOwner [

	| tab |
	self snapshotState.
	
	tab := navigationState selectedTabs first.
	self assert: tab owner isNil
]
