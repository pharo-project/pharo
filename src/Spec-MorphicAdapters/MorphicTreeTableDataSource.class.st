"
I'm a dataSource to be used in MorphicTableAdapter to realise a TablePresenter.
I define my cells following the definition on columns (MorphicTableColumn).
I use a builder (MorphicTreeTableCellBuilder) to realise the cell content. 
"
Class {
	#name : #MorphicTreeTableDataSource,
	#superclass : #FTTreeDataSource,
	#category : #'Spec-MorphicAdapters-Table'
}

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
MorphicTreeTableDataSource >> headerColumn: column [

	column id ifNil: [ ^ nil ].
	^ FTCellMorph new 
		listCentering: #left;
		addMorph: column id asMorph;
		yourself
]

{ #category : #testing }
MorphicTreeTableDataSource >> isFirstColumn: column [
	
	^ (table columns indexOf: column) = 1
]

{ #category : #accessing }
MorphicTreeTableDataSource >> rootItem: anItem [

	super rootItem: anItem.
	self addSourceToRootItem
]
