"
I am table data source which items are all initially expanded.
I maintain list of collapsed items and compute items position according to it (row indexes in full table).

Internal Representation and Key Implementation Points.

    Instance Variables
	collapsedItems:		<SortedCollection of: ClyDataSourceItem>	sorted by item position
"
Class {
	#name : #ClyExpandedDataSource,
	#superclass : #ClyDataSource,
	#instVars : [
		'collapsedItems'
	],
	#category : #'Calypso-Browser-DataSource'
}

{ #category : #controlling }
ClyExpandedDataSource >> collapse: aDataSourceItem [
	queryView changeStateBy: [
		collapsedItems removeAllSuchThat: [ :each |
			aDataSourceItem hasChildAt: each position].
		collapsedItems add: aDataSourceItem.
	
		self expansionChanged]
]

{ #category : #accessing }
ClyExpandedDataSource >> collapsedItems [
	^collapsedItems
]

{ #category : #accessing }
ClyExpandedDataSource >> collapsedItems: items [
	collapsedItems := items
]

{ #category : #testing }
ClyExpandedDataSource >> countChildrenOf: aDataSourceItem [
	| environmentItem |
	environmentItem := aDataSourceItem browserItem.
	
	^environmentItem localHierarchySize
]

{ #category : #testing }
ClyExpandedDataSource >> definesChildren [
	^true
]

{ #category : #testing }
ClyExpandedDataSource >> doesItemHaveChildren: aDataSourceItem [
	| environmentItem |
	environmentItem := aDataSourceItem browserItem.
	
	^environmentItem localHierarchySize > 0
]

{ #category : #accessing }
ClyExpandedDataSource >> elementAt: rowIndex [
	| localIndex |
	localIndex := rowIndex.

	collapsedItems do: [:each | 
		each position = localIndex ifTrue: [ ^each ].
		(each isAfter: localIndex) ifTrue: [
			self itemCursor moveTo: localIndex.
			^ ClyDataSourceItem of: self value: self itemCursor currentItem ].
		localIndex := localIndex + each childrenCount.
	].

	self itemCursor moveTo: localIndex.
	^ClyDataSourceItem of: self value: self itemCursor currentItem
]

{ #category : #controlling }
ClyExpandedDataSource >> expand: aDataSourceItem [
	queryView changeStateBy: [
		collapsedItems remove: aDataSourceItem.
	
		self expansionChanged]
]

{ #category : #private }
ClyExpandedDataSource >> findCachedElementWith: anEnvironmentItem ifAbsent: absentBlock [

	^collapsedItems
		detect: [ :each | each actualObject == anEnvironmentItem actualObject ]
		ifNone: absentBlock
]

{ #category : #accessing }
ClyExpandedDataSource >> globalPositionOf: childDataSourceItem [

	| result |
	result := parentItem ifNil: [ 0 ] ifNotNil: [ parentItem globalPosition ].
	collapsedItems do: [ :each | 
		(each isAfter: childDataSourceItem position) ifTrue: [
			^result + childDataSourceItem position].
		result := result - each childrenCount].
	
	^result + childDataSourceItem position
]

{ #category : #initialization }
ClyExpandedDataSource >> initialize [
	super initialize.
		
	collapsedItems := SortedCollection sortBlock: [ :a :b | a position < b position ]
]

{ #category : #copying }
ClyExpandedDataSource >> initializeForBrowserStateSpanshot [
	| copy |
	super initializeForBrowserStateSpanshot.
	
	copy := collapsedItems collect: [ :each | 
		each copyForBrowserStateSnapshotOf: self ].
	collapsedItems := copy asSortedCollection: [ :a :b | a position < b position ].
]

{ #category : #testing }
ClyExpandedDataSource >> isExpanded: aDataSourceItem [

	^(collapsedItems includes: aDataSourceItem) not
	
]

{ #category : #testing }
ClyExpandedDataSource >> isInSameStateAs: anotherDataSource [

	(super isInSameStateAs: anotherDataSource) ifFalse: [ ^false ].
	
	collapsedItems size = anotherDataSource collapsedItems size ifFalse: [ ^false ].
	^collapsedItems allSatisfy: [ :myItem | 
		anotherDataSource collapsedItems anySatisfy: [ :anotherItem | 
			myItem isSameAs: anotherItem ] ]
	 
]

{ #category : #accessing }
ClyExpandedDataSource >> numberOfRows [
	^self itemCursor itemCount
		- (collapsedItems detectSum: [ :each | each childrenCount ])
]

{ #category : #private }
ClyExpandedDataSource >> updateExpandingItems [
	self updateItems: collapsedItems.
	collapsedItems := collapsedItems reject: [ :each | each isRemoved ]
]
