"
I'm a dataSource to be used in MorphicTableAdapter to realise a TablePresenter.
I define my cells following the definition on columns (MorphicTableColumn).
I use a builder (MorphicTreeTableCellBuilder) to realise the cell content. 
"
Class {
	#name : #SpMorphicTreeTableDataSource,
	#superclass : #FTTreeDataSource,
	#traits : 'SpTMorphicTableDataSourceCommons',
	#classTraits : 'SpTMorphicTableDataSourceCommons classTrait',
	#category : #'Spec2-Adapters-Morphic-Table'
}

{ #category : #'search strategies' }
SpMorphicTreeTableDataSource class >> allItemsStrategy [

	^ SpMorphicAllItemsStrategy
]

{ #category : #'search strategies' }
SpMorphicTreeTableDataSource class >> defaultStrategy [

	^ SpMorphicVisibleItemsStrategy
]

{ #category : #'search strategies' }
SpMorphicTreeTableDataSource class >> rootsOnlyStrategy [

	^ SpMorphicRootItemsStrategy
]

{ #category : #accessing }
SpMorphicTreeTableDataSource >> allShownItems [

	^ rootItem children flatCollect: [ :e | e withExpandedChildren ].
]

{ #category : #accessing }
SpMorphicTreeTableDataSource >> cellColumn: column row: rowIndex [
	| cell builder item |
	
	item := self elementAt: rowIndex.
	cell := FTIndentedCellMorph new.
	cell indentBy: (self cellIndentFor: item).
	(self isFirstColumn: column) ifTrue: [
		cell addMorphBack: (self buttonFor: item) ].

	builder := SpMorphicTreeTableCellBuilder on: self.
	builder 
		cell: cell;
		item: item data;
		visit: column model.
	
	^ cell 
]

{ #category : #accessing }
SpMorphicTreeTableDataSource >> cellIndentFor: item [ 

	^ item depth * 16
]

{ #category : #paths }
SpMorphicTreeTableDataSource >> indexFromPath: anArray [ 

	^ anArray sum
]

{ #category : #accessing }
SpMorphicTreeTableDataSource >> indexOfItem: anItem [ 
	
	^ self allShownItems indexOf: anItem
]

{ #category : #testing }
SpMorphicTreeTableDataSource >> isFirstColumn: column [
	
	^ (table columns indexOf: column) = 1
]

{ #category : #accessing }
SpMorphicTreeTableDataSource >> itemAtPath: aPath [ 
	
	^ self itemAtPath: aPath expanding: false
]

{ #category : #accessing }
SpMorphicTreeTableDataSource >> itemAtPath: aPath expanding: shouldExpandItems [
	
	| item |
	
	(aPath isNil or: [aPath isEmpty]) ifTrue: [ ^ nil ].
	item := self rootItem.
	
	aPath do: [ :index |
		(shouldExpandItems and: [item isRoot not]) ifTrue: [ item expand ].
		item children size < index ifTrue: [ ^ nil ].		
		item := item children at: index.
	].

	^ item
]

{ #category : #paths }
SpMorphicTreeTableDataSource >> pathFromIndex: anInteger [ 
	| item |
	
	item := self allShownItems at: anInteger ifAbsent: [ ^ #() ].
	^ self pathOfItem: item root: rootItem
]

{ #category : #accessing }
SpMorphicTreeTableDataSource >> pathOfItem: anItem root: aRoot [ 
	
	aRoot isExpanded ifFalse: [ ^#() ].
	
	(aRoot children includes: anItem)
		ifTrue: [ ^ { aRoot children indexOf: anItem } ].
		
	aRoot children withIndexDo: [ :aChild :index | | aSubPath |
		aSubPath := self pathOfItem: anItem root: aChild.
		aSubPath ifNotEmpty: [ ^ { index } , aSubPath ] ].
	
	^ #().
]

{ #category : #accessing }
SpMorphicTreeTableDataSource >> rootItem: anItem [

	super rootItem: anItem.
	self addSourceToRootItem
]

{ #category : #accessing }
SpMorphicTreeTableDataSource >> searchText: aString [

	^ (self class perform: (self searchStrategy, 'Strategy') asSymbol) 
		searchWith: aString 
		dataSource: self
]

{ #category : #sorting }
SpMorphicTreeTableDataSource >> sortElements: aSortFunction [

	unsortedElements ifNil: [ unsortedElements := self rootsItems ].
	self rootItem data: ((self rootItem children collect: #data) sorted: aSortFunction)
]

{ #category : #accessing }
SpMorphicTreeTableDataSource >> toString: anItem [
	"Override me if you expect something else."

	^ super toString: anItem data
]
