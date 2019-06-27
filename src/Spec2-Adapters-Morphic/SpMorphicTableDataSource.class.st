"
I'm a dataSource to be used in MorphicTableAdapter to realise a TablePresenter.
I define my cells following the definition on columns (MorphicTableColumn).
I use a builder (MorphicTableCellBuilder) to realise the cell content. 
"
Class {
	#name : #SpMorphicTableDataSource,
	#superclass : #FTSimpleDataSource,
	#traits : 'TSpMorphicTableDataSourceCommons',
	#classTraits : 'TSpMorphicTableDataSourceCommons classTrait',
	#category : #'Spec2-Adapters-Morphic-Table'
}

{ #category : #accessing }
SpMorphicTableDataSource >> cellColumn: column row: rowIndex [
	| cell builder |

	cell := FTCellMorph new.
	builder := SpMorphicTableCellBuilder on: self.
	builder 
		cell: cell;
		rowIndex: rowIndex;
		visit: column model.
		
	^ cell
]

{ #category : #accessing }
SpMorphicTableDataSource >> elements [ 
	
	^ self model 
		ifNotNil: #items
		ifNil: [ #() ]
]

{ #category : #accessing }
SpMorphicTableDataSource >> menuColumn: column row: rowIndex [
	| menuPresenter |

	menuPresenter := self model contextMenu.
	menuPresenter ifNil: [ ^ nil ].
	^ SpBindings
		value: self model application adapterBindings
		during: [ menuPresenter value buildWithSpec ]
]
