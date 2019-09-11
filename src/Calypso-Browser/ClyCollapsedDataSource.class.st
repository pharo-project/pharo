"
I am table data source which items are all initially collapsed.
I maintain list of expanded items and compute items position according to it (row indexes in full table).
 
Internal Representation and Key Implementation Points.

    Instance Variables
	expandedItems:		<SortedCollection of: ClyDataSourceItem>	sorted by item position
"
Class {
	#name : #ClyCollapsedDataSource,
	#superclass : #ClyDataSource,
	#instVars : [
		'expandedItems'
	],
	#category : #'Calypso-Browser-DataSource'
}

{ #category : #controlling }
ClyCollapsedDataSource >> close [
	super close.
	
	expandedItems do: [ :each | each childrenDataSource close  ]
]

{ #category : #controlling }
ClyCollapsedDataSource >> collapse: aDataSourceItem [
	queryView changeStateBy: [ 
		expandedItems remove: aDataSourceItem.
		aDataSourceItem childrenDataSource close.	

		self expansionChanged]
]

{ #category : #accessing }
ClyCollapsedDataSource >> countChildrenOf: aDataSourceItem [
	aDataSourceItem childrenDataSource ifNotNil: [:children | ^children numberOfRows].
	
	self definesChildren ifFalse: [ ^0 ].

	self error: 'should not happen'
]

{ #category : #testing }
ClyCollapsedDataSource >> definesChildren [
	^queryView definesTree
]

{ #category : #testing }
ClyCollapsedDataSource >> doesItemHaveChildren: aDataSourceItem [
	| childQuery |
	aDataSourceItem childrenDataSource ifNotNil: [:children | ^children isEmpty not].
	
	self definesChildren ifFalse: [ ^false ].

	childQuery := queryView queryToExpand: aDataSourceItem ifAbsent: [^false].
	
	^aDataSourceItem browserItem hasChildrenWhich: [:childType | 
		childQuery retrievesItemsOfType: childType ]
]

{ #category : #accessing }
ClyCollapsedDataSource >> elementAt: rowIndex [
	| localIndex |
	localIndex := rowIndex.	

	expandedItems do: [:each | 
		each position = localIndex ifTrue: [ ^each ].
		(each hasChildAt: localIndex) ifTrue: [
			^each childrenItemAt: localIndex].
		(each isAfter: localIndex) ifTrue: [
			self itemCursor moveTo: localIndex.
			^ ClyDataSourceItem of: self value: self itemCursor currentItem ].
		localIndex := localIndex - each childrenCount.
	].

	self itemCursor moveTo: localIndex.
	^ClyDataSourceItem of: self value: self itemCursor currentItem
]

{ #category : #controlling }
ClyCollapsedDataSource >> expand: aDataSourceItem [
	queryView changeStateBy: [
		aDataSourceItem expandChildren.
	
		expandedItems add: aDataSourceItem.

		self expansionChanged]
]

{ #category : #accessing }
ClyCollapsedDataSource >> expandedItems [
	^expandedItems
]

{ #category : #accessing }
ClyCollapsedDataSource >> expandedItems: items [
	expandedItems := items
]

{ #category : #private }
ClyCollapsedDataSource >> findCachedElementWith: anEnvironmentItem ifAbsent: absentBlock [

	^expandedItems 
		detect: [ :each | each actualObject == anEnvironmentItem actualObject ]
		ifNone: absentBlock
]

{ #category : #accessing }
ClyCollapsedDataSource >> findDataSourceSameAs: aDataSource ifNone: noneBlock [
	| found |
	(self isSameAs: aDataSource) ifTrue: [ ^self ].
	
	expandedItems do: [ :each | 
		found := each childrenDataSource findDataSourceSameAs: aDataSource ifNone: [nil].
		found ifNotNil: [ ^found ]].
		
	^noneBlock value
]

{ #category : #queries }
ClyCollapsedDataSource >> findItemsSimilarTo: dataSourceItems [

	| myItems childItems |
	myItems := super findItemsSimilarTo: dataSourceItems.
	myItems size == dataSourceItems size ifTrue: [ ^myItems ].
	
	childItems := expandedItems flatCollect: [ :each | 
		each childrenDataSource findItemsSimilarTo: dataSourceItems].
	
	^myItems, childItems
]

{ #category : #queries }
ClyCollapsedDataSource >> findItemsWhere: conditionBlock [

	| myItems childItems |
	myItems := super findItemsWhere: conditionBlock.
	
	childItems := expandedItems flatCollect: [ :each | 
		each  childrenDataSource findItemsWhere: conditionBlock].
	
	^myItems, childItems
]

{ #category : #queries }
ClyCollapsedDataSource >> findItemsWith: actualObjects [

	| myItems childItems |
	myItems := super findItemsWith: actualObjects.
	myItems size == actualObjects size ifTrue: [ ^myItems ].
	
	childItems := expandedItems flatCollect: [ :each | 
		each childrenDataSource findItemsWith: actualObjects].
	
	^myItems, childItems
]

{ #category : #controlling }
ClyCollapsedDataSource >> forceFullUpdate [
	super forceFullUpdate.
	expandedItems do: [ :each | each childrenDataSource forceFullUpdate]
]

{ #category : #accessing }
ClyCollapsedDataSource >> globalPositionOf: childDataSourceItem [

	| result |
	result := parentItem ifNil: [ 0 ] ifNotNil: [ parentItem globalPosition ].
	expandedItems do: [ :each | 
		(each isAfter: childDataSourceItem position) ifTrue: [
			^result + childDataSourceItem position].
		result := result + each childrenCount].
	
	^result + childDataSourceItem position
]

{ #category : #initialization }
ClyCollapsedDataSource >> initialize [
	super initialize.
		
	expandedItems := SortedCollection sortBlock: [ :a :b | a position < b position ]
]

{ #category : #copying }
ClyCollapsedDataSource >> initializeForBrowserStateSpanshot [
	| copy |
	super initializeForBrowserStateSpanshot.
	
	copy := expandedItems collect: [ :each | 
		each copyForBrowserStateSnapshotOf: self ].
	expandedItems := copy asSortedCollection: [ :a :b | a position < b position ]
]

{ #category : #testing }
ClyCollapsedDataSource >> isExpanded: aDataSourceItem [

	^expandedItems includes: aDataSourceItem 
]

{ #category : #testing }
ClyCollapsedDataSource >> isInSameStateAs: anotherDataSource [

	(super isInSameStateAs: anotherDataSource) ifFalse: [ ^false ].
	
	expandedItems size = anotherDataSource expandedItems size ifFalse: [ ^false ].
	^expandedItems allSatisfy: [ :myItem | 
		anotherDataSource expandedItems 
			detect: [ :anotherItem | myItem isSameAs: anotherItem ]
			ifFound: [ :anotherItem | 
				myItem childrenDataSource isInSameStateAs: anotherItem childrenDataSource ]
			ifNone: [ false ] ]
]

{ #category : #accessing }
ClyCollapsedDataSource >> numberOfRows [
	^self itemCursor itemCount 
		+ (expandedItems detectSum: [ :each | each childrenCount ])
]

{ #category : #controlling }
ClyCollapsedDataSource >> openOn: aQueryView [
	super openOn: aQueryView.

	expandedItems do: [:each | each childrenDataSource openOn: aQueryView]
]

{ #category : #controlling }
ClyCollapsedDataSource >> retrieveChildrenOf: aDataSourceItem [
	| childQuery childDataSource |
	childQuery := queryView 
		queryToExpand: aDataSourceItem 
		ifAbsent: [self error: 'cant retrieve children from ', aDataSourceItem printString].
	
	childDataSource := ClyCollapsedDataSource on: childQuery.
	childDataSource openOn: queryView.
	^childDataSource
]

{ #category : #private }
ClyCollapsedDataSource >> updateExpandingItems [
	self updateItems: expandedItems.
	expandedItems := expandedItems reject: [ :each | each isRemoved ]
]
