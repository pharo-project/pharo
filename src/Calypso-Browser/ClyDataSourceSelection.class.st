"
I represent collection of selected data source items.
You can create my instances by: 
	ClyDataSourceSelection fromRoot: aDataSource items: itemsArray
I can give you interesting information:
- size
- actualObjects of my items 
- itemsScope, the class of environment scope which my items arrange.
- lastSelectedItem, an item which user selects last time which means main selected item when multiple selection is not interesting.
- uniformActualObjects, filtered actual objects which belongs to scope of last selected item.

I can be modified using following methods which update visible table selection: 
- selectItems: dataSourceItems. It just changes selection to given items.
- selectItemsWhich: aBlock. It queries data source for items satisfying given block criteria.
- selectItemsWith: actualObjectsArray. It queries data source to find items in underlying query result which belongs to given objects array.

Also there are filter methods which return new selection instances:
- asSelectedRoots. It return new selection which includes only root parents of my own items.
- asSelectedItemsOf: anItemTypeClass. It returns new selection which only includes items which belongs to given item type. 
I can be converted to the scope of my items: 
- asItemsScope: aTypedScope
I can create desired selection instance which responsible to restore selection on different data sources: 
- asDesiredSelection

When data source is changed I am responsible to update visible selection of the table:
	aSelection updateItemsWhichBelongsTo: aDataSource
For example when user expands tree node selected indexes should be shifted when expansion happens before selection. Same should be done when items of data source are removed or added.

Internal Representation and Key Implementation Points.

    Instance Variables
	items:		<SequenceableCollection of<ClyDataSourceItem>>
	rootDataSource:		<ClyDataSource>
"
Class {
	#name : #ClyDataSourceSelection,
	#superclass : #Object,
	#instVars : [
		'items',
		'rootDataSource'
	],
	#category : #'Calypso-Browser-DataSource'
}

{ #category : #'instance creation' }
ClyDataSourceSelection class >> fromRoot: aDataSource items: dataSourceItems [
	^self new 
		rootDataSource: aDataSource;
		items: dataSourceItems
]

{ #category : #accessing }
ClyDataSourceSelection >> actualObjects [
	^items collect: [:each | each actualObject]
]

{ #category : #converting }
ClyDataSourceSelection >> asDesiredSelection [

	| itemsCopy |
	itemsCopy := items collect: [ :each | each copy childrenDataSource: nil].
	^ClyDesiredSelection fromRoot: rootDataSource items: itemsCopy.

]

{ #category : #converting }
ClyDataSourceSelection >> asItemsScope: aTypedScope [
	| actualItems |
	actualItems := self uniformActualObjects.
	^ aTypedScope ofAll: actualItems in: rootDataSource queryEnvironment
]

{ #category : #converting }
ClyDataSourceSelection >> asSelectedItemsOf: itemTypeClass [

	| resultItems |
	self isEmpty ifTrue: [ ^self ].
	resultItems := items select: [ :each | each isBasedOnItemType: itemTypeClass ].
	resultItems = items ifTrue: [ ^self ].
	
	^self class fromRoot: self rootDataSource items: resultItems
]

{ #category : #converting }
ClyDataSourceSelection >> asSelectedParentsOf: itemTypeClass [

	| parents resultItems currentItems resultSet lastSelectedItem |
	self isEmpty ifTrue: [ ^self ].
	currentItems := items select: [ :each | each isBasedOnItemType: itemTypeClass].
	currentItems = items ifTrue: [ ^self ].
	parents := items 
		reject: [:each | each isBasedOnItemType: itemTypeClass ] 
		thenCollect: [ :each |each findParentItemOf: itemTypeClass ].
	resultSet := parents asSet.
	resultSet remove: nil ifAbsent: [].
	resultSet addAll: currentItems.
	
	
	"We want to save root of lastSelectedItem as lastSelectedItem of new selection. 
	This item should be first according to FastTable logic".
	resultItems := resultSet asOrderedCollection.
	lastSelectedItem := self lastSelectedItem.
	(resultSet includes: lastSelectedItem) ifTrue: [ 
		resultItems remove: lastSelectedItem.
		resultItems addFirst: lastSelectedItem].	
	
	^self class fromRoot: self rootDataSource items: resultItems
]

{ #category : #converting }
ClyDataSourceSelection >> asSelectedRoots [

	| roots lastSelectedRoot resultItems |
	self isEmpty ifTrue: [ ^self ].
	roots := items collect: [ :each | each rootParentItem ] as: Set.
	roots = items ifTrue: [ ^self ].
	
	"We want to save root of lastSelectedItem as lastSelectedItem of new selection. 
	This item should be first according to FastTable logic"
	lastSelectedRoot := self lastSelectedItem rootParentItem.
	roots remove: lastSelectedRoot.
	resultItems := OrderedCollection with: lastSelectedRoot.
	resultItems addAll: roots.
	
	^self class fromRoot: self rootDataSource items: resultItems
]

{ #category : #controlling }
ClyDataSourceSelection >> beEmpty [

	self selectItems: { }
]

{ #category : #copying }
ClyDataSourceSelection >> copyForBrowserStateSnapshotOf: aDataSource [

	| copy |
	copy := self asDesiredSelection.
	copy rootDataSource: aDataSource.
	copy items: (items collect: [ :each | each copyForBrowserStateSnapshotOf: aDataSource]).
	^copy	
]

{ #category : #accessing }
ClyDataSourceSelection >> detachedItems [
	"Selection manages own items in the way that they should not be in long use from outside.
	For example when items data source is changed my items could have null environment item inside
	accouring to selection update logic. So users should not break because of that.
	If users want to use selection items for long time (for example browser context wants) 
	then they should ask for #detachedItems"
	
	^items collect: [:each | each copy]
]

{ #category : #controlling }
ClyDataSourceSelection >> ensureVisibleLastItem [

	"in fact first selection index is last selected item"
	rootDataSource table ensureVisibleFirstSelection
]

{ #category : #accessing }
ClyDataSourceSelection >> groupItemsByType [

	| groups |
	groups := IdentityDictionary new.
	
	items do: [ :each | | group |
		group := groups at: each type ifAbsentPut: [ OrderedCollection new ].
		group add: each actualObject].
	
	^groups
]

{ #category : #accessing }
ClyDataSourceSelection >> groupItemsByTypeAndDo: typeAndItemsBlock [

	self groupItemsByType keysAndValuesDo: typeAndItemsBlock
]

{ #category : #testing }
ClyDataSourceSelection >> hasSelectedItems [
	^self isEmpty not
]

{ #category : #testing }
ClyDataSourceSelection >> includesActualObject: anObject [
	^items anySatisfy: [ :each | each includesActualObject: anObject ]
]

{ #category : #testing }
ClyDataSourceSelection >> includesItemsOf: itemTypeClass [

	^items anySatisfy: [ :each | each isBasedOnItemType: itemTypeClass ]
]

{ #category : #testing }
ClyDataSourceSelection >> isBasedOnItemType: aClass [

	^self isEmpty 
		ifTrue: [ rootDataSource isBasedOnQueryOf: aClass ]
		ifFalse: [ self lastSelectedItem isBasedOnItemType: aClass  ]
]

{ #category : #testing }
ClyDataSourceSelection >> isEmpty [
	^items isEmpty
]

{ #category : #testing }
ClyDataSourceSelection >> isMultipleSelected [
	^items size > 1
]

{ #category : #testing }
ClyDataSourceSelection >> isSameAs: anotherSelection [

	self size = anotherSelection size ifFalse: [ ^false ].
	
	items with: anotherSelection items do: [ :myItem :anotherItem | 
		myItem actualObject == anotherItem actualObject ifFalse: [ ^false ]].
	
	^true
]

{ #category : #testing }
ClyDataSourceSelection >> isSingleSelected [
	^items size = 1
]

{ #category : #accessing }
ClyDataSourceSelection >> items [
	^ items
]

{ #category : #accessing }
ClyDataSourceSelection >> items: anObject [
	items := anObject
]

{ #category : #accessing }
ClyDataSourceSelection >> lastSelectedItem [

	^items first " it is related to ordering logic in FastTable"
]

{ #category : #printing }
ClyDataSourceSelection >> printOn: aStream [
	super printOn: aStream.
	
	aStream nextPut: $(.
	items do: [ :each | aStream nextPutAll: each name; nextPutAll: '; ' ].
	items ifNotEmpty: [ aStream skip: -2 ].
	aStream nextPut: $).
]

{ #category : #controlling }
ClyDataSourceSelection >> restoreDesiredSelectionWith: newDataSourceItems silently: aBool [ 

	items := newDataSourceItems.

	self restoreTableSelectionSilently: aBool.
	
	aBool ifFalse: [self ensureVisibleLastItem]
]

{ #category : #controlling }
ClyDataSourceSelection >> restoreTableSelection [
	| actualSelectionChanged |
	actualSelectionChanged := items anySatisfy: #isRemoved.
	
	self restoreTableSelectionSilently: actualSelectionChanged not.
	
	^actualSelectionChanged 
]

{ #category : #controlling }
ClyDataSourceSelection >> restoreTableSelectionSilently: silentSelection [

	| selectionIndexes |
	items := items reject: [ :each | each isRemoved ].
	selectionIndexes := items collect: #globalPosition.
	
	self 
		setUpSelectedRows: selectionIndexes 
		in: rootDataSource table 
		silently: silentSelection
]

{ #category : #accessing }
ClyDataSourceSelection >> rootDataSource [
	^ rootDataSource
]

{ #category : #accessing }
ClyDataSourceSelection >> rootDataSource: anObject [
	rootDataSource := anObject
]

{ #category : #controlling }
ClyDataSourceSelection >> selectItems: dataSourceItems [
	
	| newSelectionIsSame |
	newSelectionIsSame := (dataSourceItems collect: #actualObject as: IdentitySet) = 
		(items collect: #actualObject as: IdentitySet).

	items := dataSourceItems.
	self restoreTableSelectionSilently: newSelectionIsSame.
	self ensureVisibleLastItem
]

{ #category : #controlling }
ClyDataSourceSelection >> selectItemsWhere: conditionBlock [

	| newItems |
	newItems := rootDataSource findItemsWhere: conditionBlock.
	self selectItems: newItems
]

{ #category : #controlling }
ClyDataSourceSelection >> selectItemsWith: actualObjects [

	| newItems |
	newItems := rootDataSource queryView findItemsWith: actualObjects.
	self selectItems: newItems
]

{ #category : #private }
ClyDataSourceSelection >> setUpSelectedRows: indexes in: aTableMorph silently: silentSelection [
	silentSelection
		ifTrue: [ aTableMorph basicSelectIndexes: indexes ]
		ifFalse: [ "we should prevent ignoring new selection if indexes are still same"
			aTableMorph basicSelectIndexes: #(-1000).
			aTableMorph selectIndexes: indexes ]
]

{ #category : #accessing }
ClyDataSourceSelection >> size [
	^items size
]

{ #category : #accessing }
ClyDataSourceSelection >> uniformActualObjects [

	| lastItem |
	self isEmpty ifTrue: [ ^#() ].
	lastItem := self lastSelectedItem.
	
	^(items allSatisfy: [:each | each isSameKindAs: lastItem ]) 
		ifTrue: [ items collect: #actualObject]
		ifFalse: [{lastItem actualObject}]
]

{ #category : #controlling }
ClyDataSourceSelection >> updateIfDirty [
	
	rootDataSource isDirty ifTrue: [ rootDataSource runUpdate ].
	items do: [ :each | each ownerDataSource isDirty 
		ifTrue: [ each ownerDataSource runUpdate] ]
]

{ #category : #controlling }
ClyDataSourceSelection >> updateItemsWhichBelongsTo: aDataSource [
	| relatedItems actualSelectionChanged |
	relatedItems := items select: [ :each | each belongsToDataSource: aDataSource].
	aDataSource updateItems: relatedItems.
	
	actualSelectionChanged := relatedItems anySatisfy: #isRemoved.
	
	self restoreTableSelectionSilently: actualSelectionChanged not.
	
	^actualSelectionChanged 
]
