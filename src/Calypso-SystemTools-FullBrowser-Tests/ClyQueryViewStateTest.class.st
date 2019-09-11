Class {
	#name : #ClyQueryViewStateTest,
	#superclass : #ClyNavigationHistoryTestCase,
	#instVars : [
		'viewDataSource',
		'queryView'
	],
	#category : #'Calypso-SystemTools-FullBrowser-Tests'
}

{ #category : #running }
ClyQueryViewStateTest >> selectedItem [
	^viewDataSource elementAt: 1  "it is self class"
]

{ #category : #running }
ClyQueryViewStateTest >> setUp [
	| packages |
	super setUp.
	packages := ClyAllPackagesQuery 
		from: ClyPackageScope of: ClyClass7WithTag1FromP5 package in: environment.
	"it is P5WithTag package which allow items expansion"
	viewDataSource := ClyCollapsedDataSource on: packages.
	queryView := ClyQueryViewMorph for: browser.
	queryView
		treeStructure: {(RPackage -> #prepareClassGroupQueryFrom:in:)}.
	queryView dataSource: viewDataSource.
	queryView selection selectItems: {self selectedItem}
]

{ #category : #running }
ClyQueryViewStateTest >> snapshotState [

	navigationState := queryView snapshotState.
]

{ #category : #tests }
ClyQueryViewStateTest >> testApplyingToQueryViewShouldCreateOpenedDataSource [

	| newView |
	self snapshotState.
	newView := ClyQueryViewMorph for: browser.
	navigationState applyTo: newView.
	 
	self assert: newView dataSource ~~ navigationState dataSource.
	self deny: newView dataSource isClosed.
	self assert: newView dataSource queryView equals: newView.
	self assert: newView dataSource table notNil
]

{ #category : #tests }
ClyQueryViewStateTest >> testApplyingToQueryViewShouldRestoreExpandedItems [

	| expandedItem newView |
	self selectedItem expand.
	
	self snapshotState.
	newView := ClyQueryViewMorph for: browser.
	navigationState applyTo: newView.

	expandedItem := navigationState dataSource expandedItems first.
	self assert: expandedItem childrenDataSource isClosed description: 'applying should not open data source snapshot'.
	
	self assert: newView dataSource expandedItems size equals: 1.	
	expandedItem := newView dataSource expandedItems first.
	self assert: (expandedItem isSameAs: self selectedItem).
	self deny: expandedItem childrenDataSource isClosed.
	self assert: expandedItem childrenDataSource queryView equals: newView.
	self assert: expandedItem childrenDataSource table notNil
]

{ #category : #tests }
ClyQueryViewStateTest >> testDataSourceIsClosed [

	self snapshotState.
	
	self assert: navigationState dataSource isClosed
]

{ #category : #tests }
ClyQueryViewStateTest >> testDataSourceWithExpandedItems [

	self selectedItem expand.
	
	self snapshotState.
	
	self assert: (navigationState dataSource expandedItems first isSameAs: viewDataSource expandedItems first)
]

{ #category : #tests }
ClyQueryViewStateTest >> testDataSourceWithoutQueryView [

	self snapshotState.
	
	self assert: navigationState dataSource queryView isNil
]

{ #category : #tests }
ClyQueryViewStateTest >> testDataSourceWithoutTable [

	self snapshotState.
	
	self assert: navigationState dataSource table isNil
]

{ #category : #tests }
ClyQueryViewStateTest >> testExpandedDataSourceIsClosed [

	| expandedItem |
	self selectedItem expand.
	
	self snapshotState.
	
	expandedItem := navigationState dataSource expandedItems first.
	self assert: expandedItem childrenDataSource isClosed
]

{ #category : #tests }
ClyQueryViewStateTest >> testExpandedDataSourceWithRetrievedExpandedItem [

	| expandedItem |
	self selectedItem expand.
	
	self snapshotState.
	
	expandedItem := navigationState dataSource expandedItems first.
	self assert: expandedItem childrenDataSource parentItem equals: expandedItem
]

{ #category : #tests }
ClyQueryViewStateTest >> testExpandedDataSourceWithoutQueryView [

	| expandedItem |
	self selectedItem expand.
	
	self snapshotState.
	
	expandedItem := navigationState dataSource expandedItems first.
	self assert: expandedItem childrenDataSource queryView isNil
]

{ #category : #tests }
ClyQueryViewStateTest >> testExpandedDataSourceWithoutTable [

	| expandedItem |
	self selectedItem expand.
	
	self snapshotState.
	
	expandedItem := navigationState dataSource expandedItems first.
	self assert: expandedItem childrenDataSource table isNil
]

{ #category : #tests }
ClyQueryViewStateTest >> testExpandedItemWithRetrievedOwnerDataSource [

	| expandedItem |
	self selectedItem expand.
	
	self snapshotState.
	
	expandedItem := navigationState dataSource expandedItems first.
	self assert: expandedItem ownerDataSource equals: navigationState dataSource
]

{ #category : #tests }
ClyQueryViewStateTest >> testRetrievesCurrentSelection [

	self snapshotState.
	
	self assert: (navigationState selection lastSelectedItem isSameAs: self selectedItem)
]

{ #category : #tests }
ClyQueryViewStateTest >> testSelectedChildInExpandedItemWithRetrievedExpandedDataSource [

	| selectedItem retrievedSelectedItem |
	self selectedItem expand.
	selectedItem := self selectedItem childrenDataSource elementAt: 1. "some class group".
	queryView selection selectItems: { selectedItem }.
	self snapshotState.
	
	retrievedSelectedItem := navigationState selection items first.
	self assert: retrievedSelectedItem ownerDataSource equals: navigationState dataSource expandedItems first childrenDataSource
]

{ #category : #tests }
ClyQueryViewStateTest >> testSelectedItemPointsToRetrievedDataSource [

	| item |
	self snapshotState.
	
	item := navigationState selection items first.
	self assert: item ownerDataSource equals: navigationState dataSource
]

{ #category : #tests }
ClyQueryViewStateTest >> testSelectionWithRetrievedDataSourceCopy [

	self snapshotState.
	
	self assert: navigationState selection rootDataSource equals: navigationState dataSource
]

{ #category : #tests }
ClyQueryViewStateTest >> testViewDataSourceIsNotDetatchedAfterSnapshoting [

	self snapshotState.
	
	self assert: (viewDataSource queryResult hasSubscriber: viewDataSource)
]
