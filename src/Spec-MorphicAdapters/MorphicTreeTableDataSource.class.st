"
I'm a dataSource to be used in MorphicTableAdapter to realise a TablePresenter.
I define my cells following the definition on columns (MorphicTableColumn).
I use a builder (MorphicTreeTableCellBuilder) to realise the cell content. 
"
Class {
	#name : #MorphicTreeTableDataSource,
	#superclass : #FTTreeDataSource,
	#traits : 'TMorphicTableDataSourceCommons',
	#classTraits : 'TMorphicTableDataSourceCommons classTrait',
	#category : #'Spec-MorphicAdapters-Table'
}

{ #category : #accessing }
MorphicTreeTableDataSource >> allShownItems [

	^ rootItem children flatCollect: [ :e | e withExpandedChildren ].
]

{ #category : #accessing }
MorphicTreeTableDataSource >> cellColumn: column row: rowIndex [
	| cell builder item |
	
	item := self elementAt: rowIndex.
	cell := FTIndentedCellMorph new.
	cell indentBy: (self cellIndentFor: item).
	(self isFirstColumn: column) ifTrue: [
		cell addMorphBack: (self buttonFor: item) ].

	builder := MorphicTreeTableCellBuilder on: self.
	builder 
		cell: cell;
		item: item data;
		visit: column model.
	
	^ cell 
]

{ #category : #accessing }
MorphicTreeTableDataSource >> cellIndentFor: item [ 

	^ item depth * 16
]

{ #category : #accessing }
MorphicTreeTableDataSource >> indexOfItem: anItem [ 
	
	^ self allShownItems indexOf: anItem
]

{ #category : #testing }
MorphicTreeTableDataSource >> isFirstColumn: column [
	
	^ (table columns indexOf: column) = 1
]

{ #category : #accessing }
MorphicTreeTableDataSource >> itemAtPath: aPath [ 
	
	^ self itemAtPath: aPath expanding: false
]

{ #category : #accessing }
MorphicTreeTableDataSource >> itemAtPath: aPath expanding: shouldExpandItems [
	
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
MorphicTreeTableDataSource >> pathFromIndex: anInteger [ 
	| item |
	
	item := self allShownItems at: anInteger ifAbsent: [ ^ #() ].
	^ self pathOfItem: item root: rootItem
]

{ #category : #accessing }
MorphicTreeTableDataSource >> pathOfItem: anItem root: aRoot [ 
	
	aRoot isExpanded ifFalse: [ ^#() ].
	
	(aRoot children includes: anItem)
		ifTrue: [ ^ { aRoot children indexOf: anItem } ].
		
	aRoot children withIndexDo: [ :aChild :index | | aSubPath |
		aSubPath := self pathOfItem: anItem root: aChild.
		aSubPath ifNotEmpty: [ ^ { index } , aSubPath ] ].
	
	^ #().
]

{ #category : #accessing }
MorphicTreeTableDataSource >> rootItem: anItem [

	super rootItem: anItem.
	self addSourceToRootItem
]
